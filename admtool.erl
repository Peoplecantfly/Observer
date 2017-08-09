-module(admtool).
-behaviour(gen_server).

%% API
-export([start/0]).
-export([start/1]).

-export([get_config/1]).
-export([ping/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(CONFIG_FILENAME, "./admtool.cfg").
-define(SERVER, ?MODULE).

-record(config, {
			cookie = "123"       :: string(),
			nodes = ""           :: list(),
			apps = [observer]    :: [atom()]

		}).

start(Args) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
start() ->
	start([]).


ping() ->
	gen_server:cast(?SERVER, ping).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_cfg(Params) ->
	parse_cfg(Params,  #config{}).
parse_cfg([], Cfg) ->
	Cfg;
parse_cfg([{cookie, Val} | Rest], Cfg) ->
	NewCfg = Cfg #config{cookie = Val},
	parse_cfg(Rest, NewCfg);
parse_cfg([{nodes, Val} | Rest], Cfg) ->
	NewCfg = Cfg #config{nodes = Val},
	parse_cfg(Rest, NewCfg);
parse_cfg([{apps, Val} | Rest], Cfg) ->
	NewCfg = Cfg #config{apps = Val},
	parse_cfg(Rest, NewCfg);
parse_cfg([{_Key, _Value} | Env], Cfg) ->
	parse_cfg(Env, Cfg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_config(CfgName) ->
	CfgFile = case application:get_env(admtool, cfg) of
		{ok, Name} -> Name;
		_ -> ?CONFIG_FILENAME
	end,
	case file:consult(CfgFile) of
		{ok, Params} ->
			PP = proplists:get_value(CfgName, Params, []),
			parse_cfg(PP);
		{error, Reason} ->
			io:format("Can't read config ~p (reason ~p)~n", [CfgFile, Reason]),
			#config{}
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Args) ->
	%net_kernel:start('master'),
	%process_flag(trap_exit, true),
	[CfgName | _] = Args,
	Name = string:to_upper(atom_to_list(CfgName)),
	io:format("~nConnect to  ~p~n", [Name]),
	Cfg = get_config(CfgName),
	timer:apply_after(100, ?MODULE, ping, []),
	{ok, Cfg}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, From, State) ->
	Reply = {ok, From},
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(ping, #config {cookie = Cookie, nodes = Nodes, apps = Apps} = State) ->
	% net_kernel:start(['master', shortnames]),
	% net_kernel:monitor_nodes(true),

	io:format("Cookie:      ~p~n", [Cookie]),
	erlang:set_cookie(node(), list_to_atom(Cookie)),
	inet_db:set_lookup([file, dns, native]),
	F = fun(NodeInfo, Acc) ->
		Node = proplists:get_value(name, NodeInfo),
		IP = proplists:get_value(ip, NodeInfo),
		io:format("Node:        ~p~n", [Node]),
		io:format("IP:          ~p~n", [IP]),
		[_N, Host] = string:tokens(Node, "@"),
		Atom = list_to_atom(Node),
		io:format("Resolve >>>> ~p~n", [Host]),
		inet_db:add_host(IP, [Host]),
		io:format("Ping to >>>> ~p~n", [Atom]),
		 case net_adm:ping(Atom) of
			 pang -> io:format("Status  >>>> PANG~n"),
			 Acc or false;
			 pong ->
				io:format("Status  >>>> PONG~n"),
				true
		 end
	end,
	case lists:foldl(F, false, Nodes) of
		true ->
			lists:foreach(fun(App) -> App:start() end, Apps);
		_ -> pang
	end,
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

