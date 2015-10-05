## 01 ## Assign IFRIS class labels to all applications in tls201_appln
SELECT application as appln_id, ipc as ipc_class, ipc_share, sfields as ifris_class FROM (
  SELECT * FROM (
    SELECT ta.application as application, ta.ipc as ipc, ipc_count/ipc_tot as ipc_share, t3.sfields as sfields, t3.ipc as yes_ipc, t3.not_ipc as not_ipc, t3.not_appln_id as exclude, t4.classes as other_classes FROM (
      SELECT t1.appln_id as application, t1.ipc_class_symbol as ipc, count(t2.appln_id) as ipc_count, sum(ipc_count) over (PARTITION BY application) as ipc_tot
        FROM thesis.tls209_appln_ipc t1

        FULL OUTER JOIN EACH
       (SELECT appln_id, FROM thesis.tls209_appln_ipc GROUP EACH BY appln_id )  t2 
          ON t1.appln_id = t2.appln_id
            GROUP EACH BY application, ipc
          ) AS ta

      LEFT JOIN EACH (
      SELECT appln_id, GROUP_CONCAT(ipc_class_symbol) as classes FROM [thesis.tls209_appln_ipc] 
        GROUP EACH BY appln_id) t4
      ON ta.application = t4.appln_id

      CROSS JOIN  thesis.ifris_ipc_concordance t3
      WHERE ta.ipc CONTAINS t3.ipc
  ) as tx
   WHERE (not ipc contains not_ipc or not_ipc is null) 
   AND (not other_classes contains exclude or exclude is null or other_classes is null)
)


## 02 ## Select priorities like De Rassenfosse (2013)
SELECT t1.appln_id as appln_id   
 FROM thesis.tls201_appln t1
  LEFT OUTER JOIN EACH thesis.tls204_appln_prior t2 ON t1.appln_id = t2.appln_id
  LEFT OUTER JOIN EACH thesis.tls211_pat_publn t5 ON t1.appln_id = t5.appln_id
 WHERE (t1.appln_kind LIKE '%A%' OR t1.appln_kind LIKE '%W%')
  AND t1.internat_appln_id = 0
  AND t2.appln_id is null
  AND t5.publn_nr is not null 
  AND t5.publn_kind !='D2';
  
## 03 ## select ifris class count for each class, for each REGPAT region
SELECT
  t1.Reg_code AS reg_code,
  t2.ifris_class AS ifris_class,
  COUNT(t1.Appln_id) AS ifris_occur
FROM
  thesis.regpat_all_inv t1
INNER JOIN EACH (
  SELECT
    appln_id,
    ifris_class
  FROM
    thesis.appln_ipc_ifris
  ) t2
ON
  t1.Appln_id = t2.appln_id
GROUP EACH BY reg_code, ifris_class
ORDER BY reg_code, ifris_class

##03b## added yearfilter to query 03

SELECT
  t1.Reg_code AS reg_code,
  t2.ifris_class AS ifris_class,
  COUNT(t1.Appln_id) AS ifris_occur
FROM
  thesis.regpat_all_inv t1
INNER JOIN EACH (
  SELECT
    appln_id,
    ifris_class
  FROM
    thesis.appln_ipc_ifris
  ) t2
ON
  t1.Appln_id = t2.appln_id
INNER JOIN EACH thesis.tls201_appln t3 on t1.Appln_id = t3.appln_id 
WHERE t3.prior_earliest_year BETWEEN 2001 and 2005
GROUP EACH BY reg_code, ifris_class
ORDER BY reg_code, ifris_class

##03c## added sourcefilter to query 3b


SELECT
  t1.Reg_code AS reg_code,
  t2.ifris_class AS ifris_class,
  COUNT(t1.Appln_id) AS ifris_occur
FROM
  thesis.regpat_all_inv t1
INNER JOIN EACH (
  SELECT
    appln_id,
    ifris_class
  FROM
    thesis.appln_ipc_ifris
  ) t2
ON
  t1.Appln_id = t2.appln_id
INNER JOIN EACH thesis.tls201_appln t3 on t1.Appln_id = t3.appln_id 
WHERE t3.prior_earliest_year BETWEEN 2001 and 2005
AND source = 'PCT'
AND reg_code NOT LIKE '%ZZ%'
AND reg_code NOT LIKE ''
GROUP EACH BY reg_code, ifris_class
ORDER BY reg_code, ifris_class


## 04 ## Count the number of (unique) applications in each low-level region
SELECT Reg_code, count(distinct Appln_id) as npatents from thesis.regpat_all_inv
  GROUP BY Reg_code


## 05 ## Count the numer of (unique) applications in each up-level region
SELECT up_reg_code, up_reg_label, COUNT(distinct t2.appln_id) AS npatents FROM thesis.regpat_regions t1
  INNER JOIN EACH thesis.regpat_all_inv t2 on t1.reg_code = t2.Reg_code
  GROUP BY up_reg_code, up_reg_label

## 06 ## Determine 'regionality' of all applications with region information available
SELECT t1.citing_appln_id, 
count(distinct t3.Appln_id) as Ncited_applns_tot, 
count(distinct t4.Appln_id) as Ncited_applns_reg, 

(count(distinct t4.Appln_id)/count(distinct t3.Appln_id)) AS regionality

FROM thesis.appln_citation t1
INNER JOIN EACH thesis.regpat_all_inv t2 on t1.citing_appln_id = t2.Appln_id
INNER JOIN EACH thesis.regpat_all_inv t3 on t1.cited_appln_id = t3.Appln_id
LEFT JOIN EACH thesis.regpat_all_inv t4 on t1.cited_appln_id = t4.Appln_id AND t2.Reg_code = t4.Reg_code

  WHERE t3.Appln_id IS NOT NULL
  GROUP EACH BY t1.citing_appln_id

## 07 ## Get appln_ids with prior_earliest_year
SELECT t1.appln_id as appln_id, prior_earliest_year AS year FROM thesis.tls201_appln t1
INNER JOIN EACH thesis.regpat_all_inv t2 on t1.appln_id = t2.Appln_id
                            GROUP EACH BY appln_id, year
                            
## 08 ## Get applicant type for all applications (by joining with eee-ppat)
Select t2.appln_id as appln_id, CASE 
  WHEN count(DISTINCT t1.sector) > 1 THEN 'MULTI'
  ELSE FIRST(t1.sector) END
  as applicant_type, count(DISTINCT t1.sector) as count 
  from thesis.eee_ppat t1
  INNER JOIN EACH thesis.tls207_pers_appln_sub t2 on t1.person_id = t2.person_id
  WHERE t1.sector IN ('INDIVIDUAL', 'COMPANY',  'UNIVERSITY', 'GOV NON-PROFIT', 'HOSPITAL')
 
  GROUP each BY appln_id
  order by appln_id
  
  
## 09 ## Create dummy variables for lib_domaines
SELECT t1.appln_id as appln_id, 
CASE WHEN GROUP_CONCAT(UNIQUE(t3.lib_domaines)) LIKE '%Electrical engineering%' THEN 1 ELSE 0 END AS EE,
CASE WHEN GROUP_CONCAT(UNIQUE(t3.lib_domaines)) LIKE '%Mechanical engineering%' THEN 1 ELSE 0 END AS ME,
CASE WHEN GROUP_CONCAT(UNIQUE(t3.lib_domaines)) LIKE '%Chemistry%' THEN 1 ELSE 0 END AS Ch,
CASE WHEN GROUP_CONCAT(UNIQUE(t3.lib_domaines)) LIKE '%Instruments%' THEN 1 ELSE 0 END AS Ins,
CASE WHEN GROUP_CONCAT(UNIQUE(t3.lib_domaines)) LIKE '%Other fields%' THEN 1 ELSE 0 END AS Oth
from thesis.appln_ipc_ifris t1
  INNER JOIN EACH thesis.appln_stats3 t2 on t1.appln_id = t2.appln_id
  LEFT JOIN EACH thesis.ifris_ipc_concordance t3 on t1.ifris_class = t3.sfields
  GROUP each BY appln_id

## 10 ## Select ifris occurrences of backward-citation pool patents
SELECT t1.appln_id as appln_id, t4.ifris_class as ifris_class, count(distinct t4.appln_id) as ifris_occur
  FROM thesis.tls211_pat_publn t1
  INNER JOIN EACH thesis.tls212_citation t2 on t1.pat_publn_id = t2.pat_publn_id
  INNER JOIN EACH thesis.tls211_pat_publn t3 on t2.cited_pat_publn_id = t3.pat_publn_id
  INNER JOIN EACH thesis.appln_ipc_ifris t4 on t3.appln_id = t4.appln_id
GROUP EACH BY appln_id, ifris_class


## 101 ## results select
SELECT t1.appln_id as appln_id,
 t1.tech_rad as rad_rs_uw,
 t2.tech_rad as rad_rs_w,
 1-(t3.tech_coh) as rad_mean,
 t4.Reg_code as reg_code,
 t5.region_div as reg_div_rs,
 t5.avg_rel_dens as reg_avg_rel_dens

FROM thesis.appln_tech_rad_jaccard10year t1
 INNER JOIN EACH thesis.appln_tech_rad_jaccard_10_year t2 on t1.appln_id = t2.appln_id
 INNER JOIN EACH thesis.appln_tech_coherence t3 on t1.appln_id = t3.appln_id
 INNER JOIN EACH thesis.regpat_all_inv t4 on t1.appln_id = t4.Appln_id
 INNER JOIN thesis.reg_stats2 t5 on t4.Reg_code = t5.reg_code

WHERE RAND() < 1000000/14000000



