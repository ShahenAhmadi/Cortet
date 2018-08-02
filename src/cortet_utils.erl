-module(cortet_utils).

%% API
-export([reform_local_date/2,
         get_rand_range/2]).

-include("cortet.hrl").

%%%===================================================================
%%% API
%%%===================================================================
reform_local_date('YYYY-MM-DD', _Timezone) ->
    {{Year, Month, Day}, _} = calendar:local_time(),
    DD = if
             Day < 10 ->
                 "0" ++ integer_to_list(Day);
             true ->
                 integer_to_list(Day)
         end,
    MM = if
             Month < 10 ->
                 "0" ++ integer_to_list(Month);
             true ->
                 integer_to_list(Month)
         end,
    integer_to_list(Year) ++ "-" ++ MM ++ "-" ++ DD;

reform_local_date('Elastic', Timezone) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Ss = if
             Sec < 10 ->
                 "0" ++ integer_to_list(Sec);
             true ->
                 integer_to_list(Sec)
         end,
    Mm = if
             Min < 10 ->
                 "0" ++ integer_to_list(Min);
             true ->
                 integer_to_list(Min)
	 end,
    Hh = if
             Hour < 10 ->
                 "0" ++ integer_to_list(Hour);
             true ->
                 integer_to_list(Hour)
	 end,
    DD = if
             Day < 10 ->
                 "0" ++ integer_to_list(Day);
             true ->
                 integer_to_list(Day)
	 end,
    MM = if
             Month < 10 ->
                 "0" ++ integer_to_list(Month);
             true ->
                 integer_to_list(Month)
         end,
    integer_to_list(Year) ++ "-" ++ MM ++ "-" ++ DD
        ++ "T" ++ Hh ++ ":" ++ Mm ++ ":" ++ Ss ++ Timezone.

get_rand_range(From, To) ->
    crypto:rand_uniform(From, To).
