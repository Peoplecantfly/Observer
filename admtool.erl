-module(admtool).
-behaviour(gen_server).

%% API
-export([start/0]).
-export([start/1]).


-export([get_config/0]).
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
			node = ""            :: string(),
			ip = {127,0,0,1}     :: inet:ip_address(),
			apps = [observer]    :: [atom()]

		}).

start(Args) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).
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
parse_cfg([{node, Val} | Rest], Cfg) ->
	NewCfg = Cfg #config{node = Val},
	parse_cfg(Rest, NewCfg);
parse_cfg([{ip, Val} | Rest], Cfg) ->
	NewCfg = Cfg #config{ip = Val},
	parse_cfg(Rest, NewCfg);

parse_cfg([{apps, Val} | Rest], Cfg) ->
	NewCfg = Cfg #config{apps = Val},
	parse_cfg(Rest, NewCfg);
parse_cfg([{_Key, _Value} | Env], Cfg) ->
	parse_cfg(Env, Cfg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_config() ->
	CfgFile = case application:get_env(admtool, cfg) of
		{ok, Name} -> Name;
		_else -> ?CONFIG_FILENAME
	end,
	case file:consult(CfgFile) of
		{ ok, Params } -> parse_cfg(Params);
		{ error, Reason } ->
			io:format("Can't read http config ~p (reason ~p)~nUsing default config~n", [CfgFile, Reason]),
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
init(_Args) ->
	%net_kernel:start('master'),
	%process_flag(trap_exit, true),
	Cfg = get_config(),
	io:format("Cookie:    ~p~n", [Cfg #config.cookie]),
	io:format("Node:      ~p~n", [Cfg #config.node]),
	io:format("IP:        ~p~n", [Cfg #config.ip]),
	ping(),
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
handle_cast(ping, #config {cookie = Cookie, node = Node, apps = Apps, ip = IP} = State) ->
	% net_kernel:start(['master', shortnames]),
	% net_kernel:monitor_nodes(true),
	erlang:set_cookie(node(), list_to_atom(Cookie)),
	[_N, Host] = string:tokens(Node, "@"),
	Atom = list_to_atom(Node),
	io:format("Ping to >>>> ~p~n", [Atom]),
	case inet:gethostbyname(Host) of
		{error, _} ->
			inet_db:add_host(IP, [Host]),
			inet_db:set_lookup([file, dns, native]);
		_ -> ok
	end,

	%io:format("kernel result = ~p~n", [net_kernel:connect(list_to_atom(Node))]),
	 case net_adm:ping(Atom) of
		 pang -> io:format("Link status >>>> PANG~n");
		 pong ->
			io:format("Link status >>>> PONG~n"),
			lists:foreach(fun(App) -> App:start() end, Apps)

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


