# Starting server
erl -sname server -setcookie busters -run ecg_tests ecg_test -detached

# Starting some nodes to play with
#erl -sname compnode1   -setcookie busters -run ecg_tests client_listener -detached 
#erl -sname compnode2   -setcookie busters -run ecg_tests client_listener -detached  
#erl -sname compnode666 -setcookie busters -run ecg_tests client_listener -detached 


#Termination section
#export KILL=`ps -C beam.smp -o pid=`
#echo "Kill list: "
#echo $KILL
#kill -9 `echo $KILL`
#echo "Checking survivors: "
#ps -C beam.smp -o pid=


