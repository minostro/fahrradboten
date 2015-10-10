-module(orders_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([submit/4, status/1, cancel/1, available/0, delivered/1, details/1, assign/2, all_ids/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(order, {
          order_id           :: undefined | orders:order_id(),
          status = available :: available | completed | in_progress,
          pickup_location    :: undefined | map:vertex(),
          dropoff_location   :: undefined | map:vertex(),
          messenger          :: undefined | dispatch:messenger(),
          worth              :: undefined | billing:worth()
         }).

-record(state, {
          orders = storage_order_new() :: [{term(), #order{}}]
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

all_ids() ->
    gen_server:call(?MODULE, all_ids, timer:seconds(5)).

submit(OrderId, PickupLocation, DropOffLocation, Worth) ->
    Order = #order{
               status = available,
               order_id = OrderId,
               pickup_location = PickupLocation,
               dropoff_location = DropOffLocation,
               worth = Worth
              },
    gen_server:call(?MODULE, {submit, Order}, timer:seconds(5)).

status(OrderId) ->
    gen_server:call(?MODULE, {status, OrderId}, timer:seconds(5)).

cancel(OrderId) ->
    gen_server:call(?MODULE, {cancel, OrderId}, timer:seconds(5)).

available() ->
    gen_server:call(?MODULE, available, timer:seconds(5)).

delivered(OrderId) ->
    gen_server:call(?MODULE, {delivered, OrderId}, timer:seconds(5)).

details(OrderId) ->
    gen_server:call(?MODULE, {details, OrderId}, timer:seconds(5)).

assign(OrderId, Messenger) ->
    gen_server:call(?MODULE, {assign, OrderId, Messenger}, timer:seconds(5)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(all_ids, _From, State = #state{orders = OrderList}) ->
    OrderIds = lists:map(fun({_, #order{order_id = ID}}) -> ID end, OrderList),
    {reply, {ok, OrderIds}, State};
handle_call({assign, OrderId, Messenger}, _From, State = #state{orders = OrderList}) ->
    NewOrderList =
        case storage_find_order(OrderId, OrderList) of
            false ->
                OrderList;
            Order ->
                storage_update_order(
                  OrderId,
                  Order#order {
                    status = in_progress,
                    messenger = Messenger
                   },
                  OrderList)
        end,
    {reply, ok, State#state{orders = NewOrderList}};
handle_call({details, OrderId}, _From, State = #state{orders = OrderList}) ->
    Resp = case storage_find_order(OrderId, OrderList) of
               false ->
                   {error, unknown_order_id};
               Order ->
                   {ok,
                    {
                      Order#order.pickup_location,
                      Order#order.dropoff_location,
                      Order#order.worth
                    }
                   }
           end,
    {reply, Resp, State};
handle_call({delivered, OrderId}, _From, State = #state{orders = OrderList}) ->
    NewOrderList = case storage_find_order(OrderId, OrderList) of
                       false ->
                           OrderList;
                       Order ->
                           storage_update_order(OrderId, Order#order{status = completed}, OrderList)
                           % lists:keyreplace(OrderId, 2, OrderList,
                           %                  Order#order{status = completed})
                   end,
    {reply, ok, State#state{orders = NewOrderList}};
handle_call(available, _From, State = #state{orders = OrderList}) ->
    AvailableOrders = lists:filter(fun
                                       ({_, #order{status = available}}) -> true;
                                       (_)                          -> false
                                   end, OrderList),
    OrderIdS = lists:map(fun({_, #order{order_id = Id}}) -> Id end, AvailableOrders),
    {reply, {ok, OrderIdS}, State};
handle_call({submit, Order}, _From, State = #state{orders = OrderList}) ->
    NewOrderList = storage_add_order(Order, OrderList),
    {reply, ok, State#state{orders = NewOrderList}};
handle_call({cancel, OrderId}, _From, State = #state{orders = OrderList}) ->
    NewOrderList = storage_delete_order(OrderId, OrderList),
    {reply, ok, State#state{orders = NewOrderList}};
handle_call({status, OrderId}, _From, State = #state{orders = OrderList}) ->
    %% Note: does not handle concept of being assigned by dispatch to a
    %% messenger
    Resp = case storage_find_order(OrderId, OrderList) of
               false -> {error, unknown_order_id};
               Order ->
                   case Order#order.status of
                       available ->
                           {ok, waiting_assignment};
                       completed ->
                           {ok, delivered};
                       in_progress ->
                           {ok, {assigned, Order#order.messenger}}
                   end
           end,
    {reply, Resp, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
storage_order_new() ->
  [].

storage_add_order(Order, Storage) ->
  [{Order#order.order_id, Order} | Storage].

storage_find_order(OrderId, Storage) ->
  case lists:keyfind(OrderId, 1, Storage) of
    false -> false;
    {_, Order} -> Order
  end.

storage_update_order(OrderId, NewOrder, Storage) ->
  lists:keyreplace(OrderId, 1, Storage, {OrderId, NewOrder}).

storage_delete_order(OrderId, Storage) ->
  lists:keydelete(OrderId, 1, Storage).

