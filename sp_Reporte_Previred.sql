SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


ALTER PROCEDURE [dbo].[sp_Reporte_Previred]
 
	@NroBatchProceso INTEGER 
AS
BEGIN
		SET NOCOUNT ON

		DECLARE @Titulo VARCHAR(100)
		DECLARE @nroEmpresa INT
		DECLARE @nroProcesos VARCHAR(100)
		DECLARE @fechaDesde DATETIME
		DECLARE @fechaHasta DATETIME
		DECLARE @TipoNomina INT
		DECLARE @aux NVARCHAR(100)
		DECLARE @NroReporte INT 
		DECLARE @TipoDocuFUN INT

		
		SET @aux = (SELECT bprcparam2  FROM batch_proceso WHERE bpronro = @NroBatchProceso)
		SET @Titulo = SUBSTRING(@aux,1,CHARINDEX('@',@aux)-1) 
		SET @aux = SUBSTRING(@aux,CHARINDEX('@',@aux)+1,100) 
		SET @nroProcesos = ',' + SUBSTRING(@aux,1,CHARINDEX('@',@aux)-1) + ','
		SET @aux = SUBSTRING(@aux,CHARINDEX('@',@aux)+1,100) 
		SET @nroEmpresa = SUBSTRING(@aux,1,CHARINDEX('@',@aux)-1) 
		SET @aux = SUBSTRING(@aux,CHARINDEX('@',@aux)+1,100) 
		SET @fechaDesde = CONVERT(DATETIME,SUBSTRING(@aux,1,CHARINDEX('@',@aux)-1) )
		SET @aux = SUBSTRING(@aux,CHARINDEX('@',@aux)+1,100) 
		SET @fechaHasta = CONVERT(DATETIME,SUBSTRING(@aux,1,CHARINDEX('@',@aux)-1)) 
		SET @TipoNomina = SUBSTRING(@aux,CHARINDEX('@',@aux)+1,100) 
		SET @NroReporte = (SELECT CASE @TipoNomina
								  WHEN 1 THEN 186 
								  WHEN 2 THEN 256
								  WHEN 3 THEN 257
								  ELSE 186 
								  END)
		SET @TipoDocuFUN = (SELECT tidnro FROM tipodocu WHERE UPPER(tidsigla) = 'FUN')




		IF OBJECT_ID('tempdb..#cabliq') IS NOT NULL DROP TABLE #cabliq
		SELECT CQ.*, CONVERT(DATETIME,NULL) FechaDesde,CONVERT(DATETIME,NULL) FechaHasta, 'LIQ' Tipo, 0 Orden  INTO #cabliq
		FROM  cabliq CQ
		WHERE CHARINDEX(','+CONVERT(VARCHAR,pronro) +',',@nroProcesos)>0
	
		/* CORREGIR Codigo de Movimiento Si el empleado Ingreso dentro del MES */
		UPDATE CQ
		SET Orden = 1, 
		    FechaDesde = F.altfec
		FROM #cabliq CQ
		INNER JOIN proceso P ON P.pronro = CQ.pronro 
		INNER JOIN fases F ON F.empleado = CQ.empleado AND F.altfec BETWEEN  P.profecini AND P.profecfin
		/* FIN */
		

		IF OBJECT_ID('tempdb..#acu_liq') IS NOT NULL DROP TABLE #acu_liq
		SELECT AQ.* into #acu_liq 
		FROM acu_liq AQ with(index(cliqnro))
		INNER JOIN #cabliq CQ ON CQ.cliqnro = AQ.cliqnro

		CREATE INDEX ix_acu_liq ON #acu_liq (cliqnro,acunro)
		CREATE INDEX ix_acu_liq2 ON #acu_liq (acunro) include (cliqnro,almonto)

		IF OBJECT_ID('tempdb..#detliq') IS NOT NULL DROP TABLE #detliq
		select DQ.* into #detliq
		from detliq DQ
		INNER JOIN #cabliq CQ ON CQ.cliqnro = DQ.cliqnro

		IF OBJECT_ID('tempdb..#Temp_Liquidacion') IS NOT NULL DROP TABLE #Temp_Liquidacion

		SELECT confnrocol, empleado, ABS(SUM(Valor)) Valor
		INTO #Temp_Liquidacion
		FROM (
			SELECT pronro, confnrocol,CQ.empleado, CASE conftipo WHEN 'CO' THEN DQ.dlimonto ELSE dlicant END Valor 
			FROM #cabliq CQ 
			INNER JOIN confrep CR ON CR.repnro = @NroReporte AND CR.conftipo IN ('CO','PCO','CCO')
			INNER JOIN #detliq DQ ON DQ.cliqnro = CQ.cliqnro
			INNER JOIN concepto C ON C.concnro = DQ.concnro AND CONVERT(INT,C.conccod) = CR.confval
			UNION ALL
			SELECT pronro,confnrocol,CQ.empleado, almonto Valor 
			FROM #cabliq CQ 
			INNER JOIN confrep CR ON CR.repnro = @NroReporte AND CR.conftipo = 'AC'
			INNER JOIN #acu_liq AQ ON AQ.cliqnro = CQ.cliqnro AND AQ.acunro = CR.confval
			) AS Consulta_Liquidacion
		GROUP BY confnrocol, empleado

		CREATE INDEX ix_templiq on #Temp_Liquidacion (confnrocol,empleado)

		IF OBJECT_ID('tempdb..#Temp_Codigo') IS NOT NULL DROP TABLE #Temp_Codigo
		SELECT confnrocol, empleado, MAX(Codigo) Valor
		INTO #Temp_Codigo
		FROM (	SELECT confnrocol, empleado, nrocod Codigo
				FROM #cabliq CQ 
				INNER JOIN confrep CR ON CR.repnro = @NroReporte AND CR.conftipo = 'TE'
				INNER JOIN his_estructura HR ON HR.tenro = CR.confval AND
												HR.ternro = CQ.empleado AND
												@fechaHasta BETWEEN HR.htetdesde AND ISNULL(HR.htethasta,'31/12/2100')
				INNER JOIN estr_cod EC ON EC.estrnro = HR.estrnro 
				UNION ALL
				SELECT confnrocol, empleado, CASE WHEN ISNULL(CR.confval2,'') = '' THEN CONVERT(VARCHAR,CR.confval) ELSE CR.confval2 END  Codigo
				FROM #cabliq CQ 
				INNER JOIN confrep CR ON CR.repnro = @NroReporte AND CR.conftipo = 'CTE'
			) AS Consulta_Codigo
		GROUP BY confnrocol, empleado
			
		CREATE INDEX ix_tempcod on #Temp_Codigo (confnrocol,empleado)

		/* Creacion de 2dos Registros por Licencias */
		SET IDENTITY_INSERT #cabliq ON 
		INSERT INTO #cabliq (cliqnro, pronro, empleado, FechaDesde, FechaHasta, Tipo, Orden)
		SELECT CQ.cliqnro, 
			   CQ.pronro, 
			   CQ.empleado,
			   CASE WHEN L.elfechadesde > P.profecini THEN L.elfechadesde ELSE  P.profecini END,
			   CASE WHEN L.elfechahasta < P.profecfin THEN L.elfechahasta ELSE  P.profecfin END,
			   'LIQ',
			   3 
		FROM #cabliq CQ
		INNER JOIN proceso P ON P.pronro = CQ.pronro 
		INNER JOIN emp_lic L ON L.empleado = CQ.empleado AND (L.elfechadesde BETWEEN P.profecini AND P.profecfin OR 
															  L.elfechahasta BETWEEN P.profecini AND P.profecfin)
		SET IDENTITY_INSERT #cabliq OFF

		INSERT INTO [dbo].[rep_previred]
           ([bpronro],[ternro],[num_linea],[Titulo],[pliqnro_Desde],[pliqnro_hasta],[empnro],[rut],[DV],[Apellido],[Apellido2],[Nombres] 
           ,[sexo],[nacionalidad],[tipo_pago],[Periodo_desde],[Periodo_hasta],[renta_imp],[reg_pre],[TipTrabajador],[DiasTrab],[TipoDeLinea],[CodmovPer],[fechadesde],[fechahasta]
           ,[TramoAsigFam],[NumCargasSim],[NumCargasMat],[NumCargasInv],[AsigFam],[AsigFamRetro],[ReintCarFam],[SolicSubsidioTrabJoven] ,[CodAFP],[RentaImponibleAFP],[CotizObligAFP],[AporteSIS],[CAVoluntaAFP],[RenImpSustAFP]
           ,[TasaPact],[AportIndem],[NumPeriodos],[PeriDesdeAFP],[PeriHastaAFP],[PuesTrabPesado],[PorcCotizTrabPesa],[CotizTrabPesa],[InstAutAPV],[NumContratoAPVI],[ForPagAPV],[CotizAPV],[CotizDepConv],[CodInstAutorizadaAPVC]
		   ,[NumContratoAPVC],[FPagoAPVC],[CotizTrabajadorAPVC],[CotizEmpleadorAPVC],[RUTAfVolunt],[DVAfVolunt],[ApePatVolunt],[ApeMatVolunt],[NombVolunt],[CodMovPersVolunt],[FecDesdeVolunt],[FecHastaVolunt],[CodAFPVolunt]
		   ,[MontoCapVolunt],[MontoAhorroVolunt],[NumPerVolunt],[CodCaReg],[TasaCotCajPrev],[RentaImpIPS],[CotizObligINP],[RentImpoDesah],[CodCaRegDesah],[TasaCotDesah],[CotizDesah]
		   ,[CotizFonasa],[CotizAccTrab],[BonLeyInp],[DescCargFam],[BonosGobierno],[CodInstSal],[NumFun],[RentaImpIsapre],[MonPlanIsapre],[CotizPact],[CotizObligIsapre],[CotizAdicVolun],[MontoGarantiaSaludGES]
		   ,[CodCCAF],[RentaImponibleCCAF],[CredPerCCAF],[DescDentCCAF],[DescLeasCCAF],[DescVidaCCAF],[OtrosDesCCAF],[CotCCAFnoIsapre],[DesCarFamCCAF],[OtrosDesCCAF1],[OtrosDesCCAF2],[BonosGobiernoCCAF]
		   ,[CodigoSucursalCCAF],[CodMut],[RentaimpMut],[CotizAccTrabMut],[SucPagMut],[RentTotImp],[AporTrabSeg],[AporEmpSeg],[RutPag],[DVPag],[CentroCosto],[auxdeci])
		   
		SELECT	@NroBatchProceso,
				CQ.empleado,
				row_number() over (order by (select NULL)),
				@Titulo,
				NULL,
				NULL,
				P.empnro, 
				LEFT(RUT.nrodoc,CHARINDEX('-',RUT.nrodoc)-1),
				RIGHT(RUT.nrodoc,1),
				E.terape, E.terape2, E.ternom,
				CASE T.tersex WHEN -1 THEN 'M' ELSE 'F' END, 
				CASE NAC.nacionaldefault WHEN -1 THEN 0 ELSE 1 END, 
				@TipoNomina, 
				CONVERT(VARCHAR,MONTH(@fechaDesde)) + CONVERT(VARCHAR,YEAR(@fechaDesde)),
				CONVERT(VARCHAR,MONTH(@fechaHasta)) + CONVERT(VARCHAR,YEAR(@fechaHasta)),
				ISNULL(renta_imp.Valor,0) renta_imp,
				reg_pre.Valor reg_pre,
				TipTrabajador.Valor TipTrabajador,
				ISNULL(DiasTrab.Valor,0) DiasTrab,
				CASE CQ.orden WHEN 0 THEN 0 ELSE 1 END TipoLinea,
				CQ.Orden CodmovPer, --PENDIENTE
				CQ.FechaDesde fechadesde, 
				CQ.FechaHasta fechahasta, 
				CASE TramoAsigFam.Valor 
				WHEN 1 THEN  'A'
				WHEN 2 THEN  'B'
				WHEN 3 THEN  'C'
				ELSE 'D' END TramoAsigFam,
				ISNULL(NumCargasSim.Valor,0) NumCargasSim,
				ISNULL(NumCargasMat.Valor,0) NumCargasMat,
				ISNULL(NumCargasInv.Valor,0) NumCargasInv,
				ISNULL(AsigFam.Valor,0) AsigFam,
				ISNULL(AsigFamRetro.Valor,0) AsigFamRetro,
				ISNULL(ReintCarFam.Valor,0) ReintCarFam,
				'N',
				ISNULL(ISNULL(LCodAFP.Valor,ECodAFP.valor),0),
				ISNULL(RentaImponibleAFP.Valor,0) RentaImponibleAFP,
				ISNULL(CotizObligAFP.Valor,0) CotizObligAFP,
				ISNULL(AporteSIS.Valor,0) AporteSIS,
				ISNULL(CAVoluntaAFP.Valor,0) CAVoluntaAFP,
				ISNULL(RenImpSustAFP.Valor,0) RenImpSustAFP,
				ISNULL(TasaPact.Valor,0) TasaPact,
				ISNULL(ReintCarFam.Valor,0) ReintCarFam,
				0 NumPeriodos,
				NULL PeriDesdeAFP,
				NULL PeriHastaAFP,
				ISNULL(SUBSTRING(PuesTrabPesado.Valor,1,40),'') PuesTrabPesado,
				ISNULL(PorcCotizTrabPesa.Valor,0) PorcCotizTrabPesa,
				ISNULL(CotizTrabPesa.Valor,0) CotizTrabPesa,
				0 InstAutAPV,		--REVISAR APVI
				0 NumContratoAPVI,  --REVISAR APVI
				0 ForPagAPV,		--REVISAR APVI
				0 CotizAPV,			--REVISAR APVI
				0 CotizDepConv,		--REVISAR APVI
				0 CodInstAutorizadaAPVC, --REVISAR APVC
				0 NumContratoAPVC,	--REVISAR APVC
				0 FPagoAPVC,		--REVISAR APVC
				0 CotizTrabajadorAPVC, --REVISAR APVC
				0 CotizEmpleadorAPVC, --REVISAR APVC
				'' RUTAfVolunt,
				'' DVAfVolunt,
				'' ApePatVolunt,
				'' ApeMatVolunt,
				'' NombVolunt,
				0 CodMovPersVolunt,
				NULL FecDesdeVolunt,
				NULL FecHastaVolunt,
				0 CodAFPVolunt,
				0 MontoCapVolunt,
				0 MontoAhorroVolunt,
				0 NumPerVolunt,
				ISNULL(CodCaReg.Valor,0) CodCaReg,
				ISNULL(TasaCotCajPrev.Valor,0) TasaCotCajPrev,
				ISNULL(RentaImpIPS.Valor,0) RentaImpIPS,
				ISNULL(CotizObligINP.Valor,0) CotizObligINP,
				ISNULL(RentImpoDesah.Valor,0) RentImpoDesah,
				ISNULL(CodCaRegDesah.Valor,0) CodCaRegDesah,
				ISNULL(TasaCotDesah.Valor,0) TasaCotDesah,
				ISNULL(CotizDesah.Valor,0) CotizDesah,
				ISNULL(CotizFonasa.Valor,0) CotizFonasa,
				ISNULL(CotizAccTrab.Valor,0) CotizAccTrab,
				ISNULL(BonLeyInp.Valor,0) BonLeyInp,
				ISNULL(DescCargFam.Valor,0) DescCargFam,
				ISNULL(BonosGobierno.Valor,0) BonosGobierno,
				ISNULL(CodInstSal.Valor,0) CodInstSal,
			    ISNULL(FUN.nrodoc,'0') NumFun,
				ISNULL(RentaImpIsapre.Valor,0) RentaImpIsapre,
				ISNULL(MonPlanIsapre.Valor,0) MonPlanIsapre,
				ISNULL(CotizPact.Valor,0) CotizPact,
				ISNULL(CotizObligIsapre.Valor,0) CotizObligIsapre,
				ISNULL(CotizAdicVolun.Valor,0) CotizAdicVolun,
				ISNULL(MontoGarantiaSaludGES.Valor,0) MontoGarantiaSaludGES,
				ISNULL(CodCCAF.Valor,0) CodCCAF,
				ISNULL(RentaImponibleCCAF.Valor,0) RentaImponibleCCAF,
				ISNULL(CredPerCCAF.Valor,0) CredPerCCAF,
				ISNULL(DescDentCCAF.Valor,0) DescDentCCAF,
				ISNULL(DescLeasCCAF.Valor,0) DescLeasCCAF,
				ISNULL(DescVidaCCAF.Valor,0) DescVidaCCAF,
				ISNULL(OtrosDesCCAF.Valor,0) OtrosDesCCAF,
				ISNULL(CotCCAFnoIsapre.Valor,0) CotCCAFnoIsapre,
				ISNULL(DesCarFamCCAF.Valor,0) DesCarFamCCAF,
				ISNULL(OtrosDesCCAF1.Valor,0) OtrosDesCCAF1,
				ISNULL(OtrosDesCCAF2.Valor,0) OtrosDesCCAF2,
				ISNULL(BonosGobiernoCCAF.Valor,0) BonosGobiernoCCAF,
				ISNULL(CodigoSucursalCCAF.Valor,0) CodigoSucursalCCAF,
				ISNULL(CodMut.Valor,0) CodMut,
				ISNULL(RentaimpMut.Valor,0) RentaimpMut,
				ISNULL(CotizAccTrabMut.Valor,0) CotizAccTrabMut,
				SucPagMut.Valor SucPagMut,
				CASE WHEN (reg_pre.valor = 'IPS' AND  ISNULL(RentTotImp.Valor,0) <> 0) OR CQ.Orden = 3 THEN ISNULL(RentTotImp.Valor,0) ELSE 0 END  RentTotImp,
				CASE WHEN (reg_pre.valor = 'IPS' AND  ISNULL(RentTotImp.Valor,0) <> 0) THEN ISNULL(AporTrabSeg.Valor,0) ELSE 0 END AporTrabSeg,
				CASE WHEN (reg_pre.valor = 'IPS' AND  ISNULL(RentTotImp.Valor,0) <> 0) OR CQ.Orden = 3 THEN ISNULL(AporEmpSeg.Valor,0) ELSE 0 END AporEmpSeg,
				0 RutPag,
				NULL DVPag,
				CentroCosto.valor CentroCosto,
				0 auxdeci --ERROR NO

		FROM #cabliq CQ
		INNER JOIN proceso P						ON P.pronro = CQ.pronro
	 	INNER JOIN empleado E						ON E.ternro = CQ.empleado
	 	INNER JOIN tercero  T						ON T.ternro = CQ.empleado
		INNER JOIN ter_doc RUT						ON RUT.ternro = E.ternro AND RUT.tidnro = 1
		LEFT  JOIN ter_doc FUN						ON RUT.ternro = E.ternro AND RUT.tidnro = @TipoDocuFUN
		LEFT  JOIN nacionalidad NAC					ON NAC.nacionalnro = T.nacionalnro
		LEFT JOIN #Temp_Liquidacion renta_imp		ON renta_imp.confnrocol = 10 AND renta_imp.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo reg_pre				ON reg_pre.confnrocol = 11 AND reg_pre.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo TipTrabajador		ON TipTrabajador.confnrocol = 12 AND TipTrabajador.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion DiasTrab		ON DiasTrab.confnrocol = 13 AND DiasTrab.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion TramoAsigFam	ON TramoAsigFam.confnrocol = 18 AND TramoAsigFam.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion NumCargasSim	ON NumCargasSim.confnrocol = 19 AND NumCargasSim.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion NumCargasMat	ON NumCargasMat.confnrocol = 20 AND NumCargasMat.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion NumCargasInv	ON NumCargasInv.confnrocol = 21 AND NumCargasInv.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion AsigFam			ON AsigFam.confnrocol = 22 AND AsigFam.empleado = E.ternro AND CQ.Orden = 0 -- Solo en primera Linea
		LEFT JOIN #Temp_Liquidacion AsigFamRetro	ON AsigFamRetro.confnrocol = 23 AND AsigFamRetro.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion ReintCarFam		ON ReintCarFam.confnrocol = 24 AND ReintCarFam.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion LCodAFP			ON LCodAFP.confnrocol = 26 AND LCodAFP.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo ECodAFP				ON ECodAFP.confnrocol = 26 AND ECodAFP.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion RentaImponibleAFP ON ((RentaImponibleAFP.confnrocol = 27 AND CQ.Orden <> 3) OR
														  (RentaImponibleAFP.confnrocol = 2027 AND CQ.Orden = 3))
														AND RentaImponibleAFP.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotizObligAFP	ON CotizObligAFP.confnrocol = 28 AND CotizObligAFP.empleado = E.ternro AND CQ.Orden = 0 -- Solo en primera Linea 
		LEFT JOIN #Temp_Liquidacion AporteSIS		ON ((AporteSIS.confnrocol = 29 AND CQ.Orden <> 3) OR
														(AporteSIS.confnrocol = 2029 AND CQ.Orden = 3)) AND AporteSIS.empleado = E.ternro 

		LEFT JOIN #Temp_Liquidacion CAVoluntaAFP	ON CAVoluntaAFP.confnrocol = 30 AND CAVoluntaAFP.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion RenImpSustAFP	ON RenImpSustAFP.confnrocol = 31 AND RenImpSustAFP.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion TasaPact		ON TasaPact.confnrocol = 32 AND TasaPact.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion AportIndem		ON AportIndem.confnrocol = 33 AND AportIndem.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo PuesTrabPesado		ON PuesTrabPesado.confnrocol = 37 AND PuesTrabPesado.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion PorcCotizTrabPesa ON PorcCotizTrabPesa.confnrocol = 38 AND PorcCotizTrabPesa.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotizTrabPesa	ON CotizTrabPesa.confnrocol = 39 AND CotizTrabPesa.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo CodCaReg				ON CodCaReg.confnrocol = 62 AND CodCaReg.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion TasaCotCajPrev	ON TasaCotCajPrev.confnrocol = 63 AND TasaCotCajPrev.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion RentaImpIPS		ON RentaImpIPS.confnrocol = 64 AND RentaImpIPS.empleado = E.ternro AND CQ.Orden = 0 -- Solo en primera Linea
		LEFT JOIN #Temp_Liquidacion CotizObligINP	ON CotizObligINP.confnrocol = 65 AND CotizObligINP.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion RentImpoDesah	ON RentImpoDesah.confnrocol = 66 AND RentImpoDesah.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo CodCaRegDesah		ON CodCaRegDesah.confnrocol = 67 AND CodCaRegDesah.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion TasaCotDesah	ON TasaCotDesah.confnrocol = 68 AND TasaCotDesah.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotizDesah		ON CotizDesah.confnrocol = 69 AND CotizDesah.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotizFonasa		ON CotizFonasa.confnrocol = 70 AND CotizFonasa.empleado = E.ternro AND CQ.Orden = 0 -- Solo en primera Linea
		LEFT JOIN #Temp_Liquidacion CotizAccTrab	ON CotizAccTrab.confnrocol = 71 AND CotizAccTrab.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion BonLeyInp		ON BonLeyInp.confnrocol = 72 AND BonLeyInp.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion DescCargFam		ON DescCargFam.confnrocol = 73 AND DescCargFam.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion BonosGobierno	ON BonosGobierno.confnrocol = 74 AND BonosGobierno.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo CodInstSal			ON CodInstSal.confnrocol = 75 AND CodInstSal.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion RentaImpIsapre	ON RentaImpIsapre.confnrocol = 77 AND RentaImpIsapre.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion MonPlanIsapre	ON MonPlanIsapre.confnrocol = 78 AND MonPlanIsapre.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotizPact		ON CotizPact.confnrocol = 79 AND CotizPact.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotizObligIsapre ON CotizObligIsapre.confnrocol = 80 AND CotizObligIsapre.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotizAdicVolun	ON CotizAdicVolun.confnrocol = 81 AND CotizAdicVolun.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion MontoGarantiaSaludGES ON MontoGarantiaSaludGES.confnrocol = 82 AND MontoGarantiaSaludGES.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo CodCCAF				ON CodCCAF.confnrocol = 83 AND CodCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion RentaImponibleCCAF	ON RentaImponibleCCAF.confnrocol = 84 AND RentaImponibleCCAF.empleado = E.ternro AND CQ.Orden = 0 -- Solo en primera Linea
		LEFT JOIN #Temp_Liquidacion CredPerCCAF		ON CredPerCCAF.confnrocol = 85 AND CredPerCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion DescDentCCAF	ON DescDentCCAF.confnrocol = 86 AND DescDentCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion DescLeasCCAF	ON DescLeasCCAF.confnrocol = 87 AND DescLeasCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion DescVidaCCAF	ON DescVidaCCAF.confnrocol = 88 AND DescVidaCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion OtrosDesCCAF	ON OtrosDesCCAF.confnrocol = 89 AND OtrosDesCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotCCAFnoIsapre	ON CotCCAFnoIsapre.confnrocol = 90 AND CotCCAFnoIsapre.empleado = E.ternro AND CQ.Orden = 0 -- Solo en primera Linea
		LEFT JOIN #Temp_Liquidacion DesCarFamCCAF	ON DesCarFamCCAF.confnrocol = 91 AND DesCarFamCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion OtrosDesCCAF1	ON OtrosDesCCAF1.confnrocol = 92 AND OtrosDesCCAF1.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion OtrosDesCCAF2	ON OtrosDesCCAF2.confnrocol = 93 AND OtrosDesCCAF2.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion BonosGobiernoCCAF ON BonosGobiernoCCAF.confnrocol = 94 AND BonosGobiernoCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo CodigoSucursalCCAF	ON CodigoSucursalCCAF.confnrocol = 95 AND CodigoSucursalCCAF.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo CodMut				ON CodMut.confnrocol = 96 AND CodMut.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion RentaimpMut		ON ((RentaimpMut.confnrocol = 97 AND CQ.Orden <> 3) OR
														(RentaimpMut.confnrocol = 2097 AND CQ.Orden = 3)) AND RentaimpMut.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion CotizAccTrabMut	ON ((CotizAccTrabMut.confnrocol = 98 AND CQ.Orden <> 3) OR
														(CotizAccTrabMut.confnrocol = 2098 AND CQ.Orden = 3)) AND CotizAccTrabMut.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo SucPagMut			ON SucPagMut.confnrocol = 99 AND SucPagMut.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion RentTotImp		ON ((RentTotImp.confnrocol = 100 AND CQ.Orden <> 3) OR
														(RentTotImp.confnrocol = 2100 AND CQ.Orden = 3)) AND RentTotImp.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion AporTrabSeg		ON AporTrabSeg.confnrocol = 101 AND AporTrabSeg.empleado = E.ternro 
		LEFT JOIN #Temp_Liquidacion AporEmpSeg		ON ((AporEmpSeg.confnrocol = 102 AND CQ.Orden <> 3) OR
														(AporEmpSeg.confnrocol = 2102 AND CQ.Orden = 3)) AND AporEmpSeg.empleado = E.ternro 
		LEFT JOIN #Temp_Codigo CentroCosto			ON CentroCosto.confnrocol = 105 AND CentroCosto.empleado = E.ternro 
		ORDER BY E.ternro, P.pronro, CQ.Orden

END



