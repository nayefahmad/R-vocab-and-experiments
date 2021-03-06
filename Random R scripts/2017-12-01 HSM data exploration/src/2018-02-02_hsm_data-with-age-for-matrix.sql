
------------------------------------------------------
-- PULL DATA WITH AGE, HSDA FOR TRANSITION MATRIX 
------------------------------------------------------


SELECT [FiscalYear] --
      ,[HSMStudyID] --
      ,[PatientStudyID] --
      ,[FiscalYearAge] --
      ,[HSDA]  --
      ,[POP_SEGMENT] --
      ,[POP_SEGMENT_LAST_YEAR] --
      
FROM [HealthSystemMatrix].[dbo].[vwVCH_HSM]
WHERE FiscalYear in ('09/10')
ORDER BY FiscalYear
		, POP_SEGMENT; 