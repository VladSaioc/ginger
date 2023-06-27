// num_comm_params=0
// num_mand_comm_params=0
// num_opt_comm_params=0

// git_link=https://github.com//blob///home/au/PhD_Materials/RHUL/benchmarks/cockroach/pkg/ccl/backupccl/create_scheduled_backup_test.go#L988
typedef Mutexdef {
	chan Lock = [0] of {bool};
	chan Unlock = [0] of {bool};
	chan RLock = [0] of {bool};
	chan RUnlock = [0] of {bool};
	int Counter = 0;}
#define default true


init { 
	chan child_TestCreateBackupScheduleCollectionOverwrite9880 = [1] of {int};
	run TestCreateBackupScheduleCollectionOverwrite988(child_TestCreateBackupScheduleCollectionOverwrite9880);
	child_TestCreateBackupScheduleCollectionOverwrite9880?0;
stop_process:skip
}

proctype TestCreateBackupScheduleCollectionOverwrite988(chan child) {
	bool closed; 
	bool ok; 
	int i;
	bool state = true;
	int num_msgs;
	Mutexdef th_cfg_Settings_Cache_mu;
	run mutex_monitor(th_cfg_Settings_Cache_mu);
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */ 

proctype mutex_monitor(Mutexdef m) {
	bool locked = false;
	end: skip;
	do
	:: default ->
		if
		:: m.Counter > 0 ->
			if
			:: m.RUnlock!true;
				m.Counter = m.Counter - 1;
			:: m.RLock!true;
				m.Counter = m.Counter + 1;
			fi;
		:: locked ->
			m.Unlock!true;
			locked = false;
		:: else ->
			end1: skip;
			if
			:: m.Unlock!false;
			:: m.Lock!true -> locked =true;
			:: m.RUnlock!false;
			:: m.RLock!true -> m.Counter = m.Counter + 1;
			fi;
		fi;
	od
}
