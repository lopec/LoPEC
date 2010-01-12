%% The NEW job record.
-record(job, {
          job_id,
          program_name,  % program name to be run
          problem_type,  % default should be mapreduce
          path,          % Path to the directory containing input data
          state = free,  % [free | stopped | paused | no_tasks]
          owner,         % Name of the user who submitted job
          priority,      % for future use, not supported right now
          is_bg = false, % true if this job is a background job
          tasks_restarted = 0
	 }).

%% The record that is stored in the bg jobs table to keep track of
%% which jobs are background jobs.
-record(bg_job, {
          job_id,
          is_bg_job = true
         }).

%% The NEW task record.
-record(task, {
          task_id,
          job_id,
          program_name,
          type, % i.e. map, reduce, split, finalise
          path,
          state = free
         }). % [free | assigned | done]

%% The record that keeps track of the relations between jobs,
%% assigned tasks and nodes.
-record(assigned_tasks, {
          task_id,
          job_id,
          node_id
         }).

%% The record that stores in which table the task resides and which
%% job it belongs to.
-record(task_relations, {
          task_id,
          job_id,
          table_name
         }).

%% The record for users.
-record(user, {
	  user_name,
	  password,
	  email,
      receive_email,
	  real_name,
	  role
	 }).

%% The record describing a key in a bucket in the storage
-record(storage_key, {
          job,
          bucket,
          key
         }).
