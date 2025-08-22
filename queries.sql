
-- ============================================================
-- queries.sql — Analysis & export queries for IBM HR dataset
-- Works with schema.sql (PostgreSQL)
-- ============================================================
SET search_path = hr, public;

-- 0) Row counts & basic sanity
SELECT COUNT(*) AS total_rows,
       SUM((attrition = 'Yes')::int) AS attritions,
       ROUND(100.0 * SUM((attrition = 'Yes')::int) / COUNT(*), 2) AS attrition_pct
FROM hr_ibm_attrition;

-- 1) Attrition by Department
SELECT department,
       COUNT(*) AS n,
       SUM((attrition = 'Yes')::int) AS attritions,
       ROUND(100.0 * SUM((attrition = 'Yes')::int) / COUNT(*), 2) AS attrition_pct
FROM hr_ibm_attrition
GROUP BY department
ORDER BY attrition_pct DESC, n DESC;

-- 2) Attrition by Job Role
SELECT job_role,
       COUNT(*) AS n,
       SUM((attrition = 'Yes')::int) AS attritions,
       ROUND(100.0 * SUM((attrition = 'Yes')::int) / COUNT(*), 2) AS attrition_pct
FROM hr_ibm_attrition
GROUP BY job_role
ORDER BY attrition_pct DESC, n DESC;

-- 3) Impact of Overtime
SELECT over_time,
       COUNT(*) AS n,
       SUM((attrition = 'Yes')::int) AS attritions,
       ROUND(100.0 * SUM((attrition = 'Yes')::int) / COUNT(*), 2) AS attrition_pct
FROM hr_ibm_attrition
GROUP BY over_time
ORDER BY over_time;

-- 4) Engagement deciles vs Attrition (uses the clean view)
WITH scored AS (
  SELECT employee_number, attrition, engagement_score,
         ntile(10) OVER (ORDER BY engagement_score) AS eng_decile
  FROM hr_ibm_attrition_clean
)
SELECT eng_decile,
       COUNT(*) AS n,
       ROUND(AVG(engagement_score)::numeric, 3) AS avg_engagement,
       SUM((attrition = 'Yes')::int) AS attritions,
       ROUND(100.0 * SUM((attrition = 'Yes')::int) / COUNT(*), 2) AS attrition_pct
FROM scored
GROUP BY eng_decile
ORDER BY eng_decile;

-- 5) Tenure buckets vs Attrition
WITH b AS (
  SELECT
    CASE
      WHEN years_at_company <= 1 THEN '0–1'
      WHEN years_at_company <= 3 THEN '2–3'
      WHEN years_at_company <= 6 THEN '4–6'
      WHEN years_at_company <= 10 THEN '7–10'
      ELSE '11+'
    END AS tenure_bucket,
    attrition
  FROM hr_ibm_attrition
)
SELECT tenure_bucket,
       COUNT(*) AS n,
       SUM((attrition = 'Yes')::int) AS attritions,
       ROUND(100.0 * SUM((attrition = 'Yes')::int) / COUNT(*), 2) AS attrition_pct
FROM b
GROUP BY tenure_bucket
ORDER BY CASE tenure_bucket
           WHEN '0–1' THEN 1
           WHEN '2–3' THEN 2
           WHEN '4–6' THEN 3
           WHEN '7–10' THEN 4
           ELSE 5
         END;

-- 6) Income quartiles vs Attrition
WITH q AS (
  SELECT employee_number, attrition, monthly_income,
         ntile(4) OVER (ORDER BY monthly_income) AS income_quartile
  FROM hr_ibm_attrition
)
SELECT income_quartile,
       COUNT(*) AS n,
       ROUND(AVG(monthly_income)::numeric, 2) AS avg_income,
       SUM((attrition = 'Yes')::int) AS attritions,
       ROUND(100.0 * SUM((attrition = 'Yes')::int) / COUNT(*), 2) AS attrition_pct
FROM q
GROUP BY income_quartile
ORDER BY income_quartile;

-- 7) Marital Status × Gender cross-tab (attrition rate)
SELECT marital_status,
       gender,
       COUNT(*) AS n,
       ROUND(100.0 * SUM((attrition = 'Yes')::int) / COUNT(*), 2) AS attrition_pct
FROM hr_ibm_attrition
GROUP BY marital_status, gender
ORDER BY marital_status, gender;

-- 8) Modeling input view (export-ready)
--    Minimal set of predictors aligned with your R scripts
DROP VIEW IF EXISTS v_model_input;
CREATE VIEW v_model_input AS
SELECT
  employee_number,
  (attrition = 'Yes')::int           AS attrition,          -- target
  age,
  monthly_income,
  years_at_company,
  CASE WHEN LOWER(over_time) = 'yes' THEN 1 ELSE 0 END AS overtime,
  -- composite engagement feature from 1–4 scales
  (COALESCE(environment_satisfaction,0)
   + COALESCE(job_satisfaction,0)
   + COALESCE(relationship_satisfaction,0)
   + COALESCE(work_life_balance,0)
   + COALESCE(job_involvement,0)) / 5.0 AS engagement_score,
  department,
  job_role,
  marital_status
FROM hr_ibm_attrition;

-- 9) (psql) Export the modeling view to CSV:
-- \copy (SELECT * FROM v_model_input ORDER BY employee_number) TO 'data/ibm_model_input.csv' CSV HEADER
