CREATE VIEW IF NOT EXISTS AbandonRatePerDepartment AS Select
    Call.ID,
	
	Week,
	
	Department.Description,
	
	(Count( case 
				  when QueueExitTime is null then 1 
					end ) * 100.0 ) / count(*) as AbandonRate

FROM
  Call
	
INNER JOIN
	Department
	
ON Call.DepartmentID = Department.ID

GROUP BY
  Week,
	Department.ID
	
ORDER BY
  Week,
  Department.Description;