	(search-configuration 1 cycle)
	(queue 0)
	(distance 0)
	(consumption 0 0)
	(source 4354 ((1 500)) ((c1 4.0)) 0 0 0 ((-1 0)))
	(source 4355 ((1 500)) ((c1 4.0)) 0 0 0 ((-1 0)))
	(goal 4333 0 ((c1 3.0)) ((1)) 0 0 ((-1 0)))
	(assigned-tasks ((truck1 ((!drive-to truck1 4367 4354 0.0 99.0)
                                  (!load truck1 4354 99.0 5.0)))
                         (truck2 ((!drive-to truck2 4367 4354 0.0 99.0)))))
        (routes ((4367 ((4354 (1 ((c1 1.0))))
		        (4355 (1 ((c1 1.0))))
			(4365 (1 ((c1 1.0))))))
		 (4365 ((4367 (1 ((c1 1.0))))))
	         (4354 ((4333 (1 ((c1 1.0))))))
	         (4333 ((4354 (1 ((c1 1.0))))
			(4355 (1 ((c1 1.0))))
		        (4367 (1 ((c1 1.0))))))
		 (4355 ((4333 (1 ((c1 1.0))))))))

