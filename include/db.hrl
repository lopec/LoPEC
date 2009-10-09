%% The job record.
-record(job, {job_id, 
	      job_type,
	      input_path, 
	      current_state, 
	      current_progress, 
	      priority}).

%% The task record.
-record(task, {task_id,
	       job_id,
	       task_type,
	       input_path,
	       current_state,
	       priority}).

%% The record that keeps track of the relations between jobs and tasks.
-record(assigned_task, {task_id,
			job_id,
			node_id}).
