%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_call(Msg, From, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Msg, From, State) ->
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected handle_call call.~n"
                         "Message: ~p~n"
                         "From: ~p~n",
                         [?MODULE, Msg, From])),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_cast(Msg, State) ->  {noreply, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected handle_cast call.~n"
                         "Message: ~p~n",
                         [?MODULE, Msg])),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs and discards unexpected messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) -> 
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected handle_info call.~n"
                         "Info: ~p~n",
                         [?MODULE, Info])),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% Logs and discards unexpected messages.
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) -> 
    chronicler:warning(io_lib:format(
                         "~w:Received unexpected code_change call.~n"
                         "Old version: ~p~n"
                         "Extra: ~p~n",
                         [?MODULE, OldVsn, Extra])),
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% Logs and discards unexpected messages.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) -> 
    chronicler:debug(io_lib:format(
                       "~w:Received terminate call.~n"
                       "Reason: ~p~n",
                       [?MODULE, Reason])),
    ok.
