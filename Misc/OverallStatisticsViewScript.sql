CREATE VIEW IF NOT EXISTS OverallStatistics AS select
  
	Week,
	
	avg(SatisfactionScore) as SatisfactionScore,

	Count(*) as TotalCalls,

	(Avg( case 
			  when QueueExitTime is not null then QueueExitTime - QueueEntryTime 
					end )) as AnswerSpeed,

	(Count( case 
				  when QueueExitTime is null then 1 
					end ) * 100.0 ) / count(*) as AbandonRate,

	(Count(*) / 9.00 / 60.00) as CallsMinute,

	(Count ( case
			   when (QueueExitTime is not null) and ((QueueExitTime - QueueEntryTime) < 0.00208333333333333) then 1
					 end )) as CallsLess180,

	(Count( case 
			  when SatisfactionScore < 3 then 1 
					end )) as CallsLess3,

	(Count( case 
			  when SatisfactionScore < 3 then 1 
					end ) * 100.0 / count(*) ) as CallsLess3Perc					
	
FROM
  Call
	
GROUP BY
  Week;