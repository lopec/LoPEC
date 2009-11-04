-record(job_stats, { job_id,
                     %% first number corresponds to free tasks of given type
                     %% second number corresponds to assigned tasks of given type
                     %% third number corresponds to done tasks of given type
                     split = {0, 0, 0},
                     map = {0, 0, 0},
                     reduce = {0, 0, 0},
                     finalize = {0, 0, 0}
                    }).

