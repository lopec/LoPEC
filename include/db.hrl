-record(job, {job_id, 
	      callback_path, 
	      input_path, 
	      current_state, 
	      current_progress, 
	      reply_id, 
	      priority}).

-record(task, {task_id,
	       job_id,
	       task_type,
	       callback_path,
	       input_path,
	       current_state,
	       priority}).

-record(assigned_task, {task_id,
			job_id,
			node_id}).
