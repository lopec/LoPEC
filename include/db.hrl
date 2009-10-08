%% The job record.
-record(job, {job_id, 
	      job_type,
	      callback_path, 
	      input_path, 
	      current_state, 
	      current_progress, 
	      reply_id, 
	      priority}).

%% The task record.
-record(task, {task_id,
	       job_id,
	       task_type,
	       callback_path,
	       input_path,
	       current_state,
	       priority}).

%% The record that keeps track of the relations between jobs and tasks.
-record(assigned_task, {task_id,
			job_id,
			node_id}).
