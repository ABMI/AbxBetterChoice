CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (3026008,3023368,3002619,3023419,3003714,3025941,3009986,3016914,3025037,3045330,3012475,3011797,3024194,3029151,3007234,3039448,3015778,36303793,3046974,3041383,3006761,3040827,3008334,40763091,44816708,3043614,3045873,36305836,3027969,44817158,43533982,40759834,46234834,36305372,3011298,3044495,3014320,40763313,3004761,3000455,46237025,36304920,36305841,3038590)

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (3035643,3035814,3003793,3007224,3006122,3034918)

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 2 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (4023812,4149253,4051640,4295448,4205585,4016020,4014816,4269070,36684336,4016448,4016449,4017113,40484223,4242438,4223548,4016601,4016164,4140527,4016600,4030716,4014972,4011683,4219241,4312475,4204079,4078241,4226115,4295899,4327473,4267438,4254213,4209452,4275741,4070556,4266088,4238672,4138394,4332097,4238215,4348164,4348429,4352626,4018786,4018788,4296403,37396510,4196666,4221197,4248426,4163698,4164566,4186601,4030558,4078364,4031192,4001253,4212063,4166421,4195412,4184768,4162406,37016674,4021050,4019607,4020059,4170580,4198204,4011216,4028952,4140691,4016238,4104185,4183695,4311807,4180726,4275872,4235004)

) I
) C;


with primary_events (event_id, person_id, start_date, end_date, op_start_date, op_end_date, visit_occurrence_id) as
(
  -- Begin Primary Events
  select P.ordinal as event_id, P.person_id, P.start_date, P.end_date, op_start_date, op_end_date, cast(P.visit_occurrence_id as bigint) as visit_occurrence_id
  FROM
  (
    select E.person_id, E.start_date, E.end_date,
    row_number() OVER (PARTITION BY E.person_id ORDER BY E.sort_date ASC) ordinal,
    OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date, cast(E.visit_occurrence_id as bigint) as visit_occurrence_id
    FROM 
    (
      select PE.person_id, PE.event_id, PE.start_date, PE.end_date, PE.target_concept_id, PE.visit_occurrence_id, PE.sort_date FROM (
        -- Begin Measurement Criteria
        select C.person_id, C.measurement_id as event_id, C.measurement_date as start_date, DATEADD(d,1,C.measurement_date) as END_DATE,
        C.measurement_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id,
        C.measurement_date as sort_date
        from 
        (
          select m.* 
            FROM @cdm_database_schema.MEASUREMENT m
          JOIN #Codesets codesets on ((m.measurement_concept_id = codesets.concept_id and codesets.codeset_id = 0))
          JOIN #Codesets2 codesets on ((m.value_as_concept_id = codesets2.concept_id and codesets2.codeset_id = 2))
        ) C
        
        -- End Measurement Criteria
        
      ) PE
      JOIN (
        -- Begin Criteria Group
        select 0 as index_id, person_id, event_id
        FROM
        (
          select E.person_id, E.event_id 
          FROM (SELECT Q.person_id, Q.event_id, Q.start_date, Q.end_date, Q.visit_occurrence_id, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
                FROM (-- Begin Measurement Criteria
                      select C.person_id, C.measurement_id as event_id, C.measurement_date as start_date, DATEADD(d,1,C.measurement_date) as END_DATE,
                      C.measurement_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id,
                      C.measurement_date as sort_date
                      from 
                      (
                        select m.* 
                          FROM @cdm_database_schema.MEASUREMENT m
                        JOIN #Codesets codesets on ((m.measurement_concept_id = codesets.concept_id and codesets.codeset_id = 0))
                        JOIN #Codesets2 codesets on ((m.value_as_concept_id = codesets2.concept_id and codesets2.codeset_id = 2))
                      ) C

                      -- End Measurement Criteria
                ) Q
                JOIN @cdm_database_schema.OBSERVATION_PERIOD OP on Q.person_id = OP.person_id 
                and OP.observation_period_start_date <= Q.start_date and OP.observation_period_end_date >= Q.start_date
          ) E
          INNER JOIN
          (
            -- Begin Correlated Criteria
            SELECT 0 as index_id, p.person_id, p.event_id
            FROM (SELECT Q.person_id, Q.event_id, Q.start_date, Q.end_date, Q.visit_occurrence_id, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
                  FROM (-- Begin Measurement Criteria
                        select C.person_id, C.measurement_id as event_id, C.measurement_date as start_date, DATEADD(d,1,C.measurement_date) as END_DATE,
                        C.measurement_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id,
                        C.measurement_date as sort_date
                        from 
                        (
                          select m.* 
                            FROM @cdm_database_schema.MEASUREMENT m
                          JOIN #Codesets codesets on ((m.measurement_concept_id = codesets.concept_id and codesets.codeset_id = 0))
                          JOIN #Codesets2 codesets on ((m.value_as_concept_id = codesets2.concept_id and codesets2.codeset_id = 2))
                        ) C

                        -- End Measurement Criteria
                  ) Q
                  JOIN @cdm_database_schema.OBSERVATION_PERIOD OP on Q.person_id = OP.person_id 
                  and OP.observation_period_start_date <= Q.start_date and OP.observation_period_end_date >= Q.start_date
            ) P
            INNER JOIN
            (
              -- Begin Measurement Criteria
              select C.person_id, C.measurement_id as event_id, C.measurement_date as start_date, DATEADD(d,1,C.measurement_date) as END_DATE,
              C.measurement_concept_id as TARGET_CONCEPT_ID, C.visit_occurrence_id,
              C.measurement_date as sort_date
              from 
              (
                select m.* 
                  FROM @cdm_database_schema.MEASUREMENT m
                JOIN #Codesets codesets on ((m.measurement_concept_id = codesets.concept_id and codesets.codeset_id = 1))
                --- for restricting the result from specific culture
                WHERE measurement_id in (select distinct fact_id_2 from @cdm_database_schema.fact_relationship 
                                         where fact_id_1 in (select distinct m1.measurement_id from @cdm_database_schema.measurement m1 
                                                             join #Codesets codesets on ((m1.measurement_concept_id = codesets.concept_id and codesets.codeset_id = 0)) 
                                                             JOIN #Codesets2 codesets on ((m1.value_as_concept_id = codesets2.concept_id and codesets2.codeset_id = 2))
                                         ) and value_as_concept_id = 4148441)
                --- end 
              ) C
              
              
              -- End Measurement Criteria
              
            ) A on A.person_id = P.person_id  AND A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,0,P.START_DATE) AND A.START_DATE <= DATEADD(day,0,P.START_DATE)
            GROUP BY p.person_id, p.event_id
            HAVING COUNT(A.TARGET_CONCEPT_ID) >= 1
            -- End Correlated Criteria
            
          ) CQ on E.person_id = CQ.person_id and E.event_id = CQ.event_id
          GROUP BY E.person_id, E.event_id
          HAVING COUNT(index_id) = 1
        ) G
        -- End Criteria Group
      ) AC on AC.person_id = pe.person_id and AC.event_id = pe.event_id
      
    ) E
    JOIN @cdm_database_schema.observation_period OP on E.person_id = OP.person_id and E.start_date >=  OP.observation_period_start_date and E.start_date <= op.observation_period_end_date
    WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE
  ) P
  
  -- End Primary Events
  
)
SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, visit_occurrence_id
INTO #qualified_events
FROM 
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal, cast(pe.visit_occurrence_id as bigint) as visit_occurrence_id
  FROM primary_events pe
  
) QE

;

--- Inclusion Rule Inserts

create table #inclusion_events (inclusion_rule_id bigint,
person_id bigint,
event_id bigint
);

with cteIncludedEvents(event_id, person_id, start_date, end_date, op_start_date, op_end_date, ordinal) as
(
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusion_events I on I.person_id = Q.person_id and I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups
  
)
select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM cteIncludedEvents Results

;

-- date offset strategy

select event_id, person_id, 
case when DATEADD(day,0,start_date) > start_date then DATEADD(day,0,start_date) else start_date end as end_date
INTO #strategy_ends
from #included_events;


-- generate cohort periods into #final_cohort
with cohort_ends (event_id, person_id, end_date) as
(
  -- cohort exit dates
  -- End Date Strategy
  SELECT event_id, person_id, end_date from #strategy_ends
  
),
first_ends (person_id, start_date, end_date) as
(
  select F.person_id, F.start_date, F.end_date
  FROM (
    select I.event_id, I.person_id, I.start_date, E.end_date, row_number() over (partition by I.person_id, I.event_id order by E.end_date) as ordinal 
    from #included_events I
    join cohort_ends E on I.event_id = E.event_id and I.person_id = E.person_id and E.end_date >= I.start_date
  ) F
  WHERE F.ordinal = 1
)
select person_id, start_date, end_date
INTO #cohort_rows
from first_ends;

with cteEndDates (person_id, end_date) AS -- the magic
(	
  SELECT
  person_id
  , DATEADD(day,-1 * 0, event_date)  as end_date
  FROM
  (
    SELECT
    person_id
    , event_date
    , event_type
    , MAX(start_ordinal) OVER (PARTITION BY person_id ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal 
    , ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY event_date, event_type) AS overall_ord
    FROM
    (
      SELECT
      person_id
      , start_date AS event_date
      , -1 AS event_type
      , ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY start_date) AS start_ordinal
      FROM #cohort_rows
      
      UNION ALL
      
      
      SELECT
      person_id
      , DATEADD(day,0,end_date) as end_date
      , 1 AS event_type
      , NULL
      FROM #cohort_rows
    ) RAWDATA
  ) e
  WHERE (2 * e.start_ordinal) - e.overall_ord = 0
),
cteEnds (person_id, start_date, end_date) AS
(
  SELECT
  c.person_id
  , c.start_date
  , MIN(e.end_date) AS end_date
  FROM #cohort_rows c
  JOIN cteEndDates e ON c.person_id = e.person_id AND e.end_date >= c.start_date
  GROUP BY c.person_id, c.start_date
)
select person_id, min(start_date) as start_date, end_date
into #final_cohort
from cteEnds
group by person_id, end_date
;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id as cohort_definition_id, person_id, start_date, end_date 
FROM #final_cohort CO
;



TRUNCATE TABLE #strategy_ends;
DROP TABLE #strategy_ends;


TRUNCATE TABLE #cohort_rows;
DROP TABLE #cohort_rows;

TRUNCATE TABLE #final_cohort;
DROP TABLE #final_cohort;

TRUNCATE TABLE #inclusion_events;
DROP TABLE #inclusion_events;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;