-module(player).
-export([create/4, schedule_game/4, send_request/2]).

create(Name, Credits, PlayerInfo, MasterPid) ->
    timer:sleep(1000),
    schedule_game(Name, Credits, PlayerInfo, MasterPid).

schedule_game(Name, Credits, PlayerInfo, MasterPid) ->
    timer:sleep(500),
    send_request(Name, PlayerInfo),
    receive
        {game_request, OpponentName} ->
            OpponentPid = whereis(OpponentName),
            OpponentPid ! {game_request_confirmed, Name, MasterPid},
            schedule_game(Name, Credits, PlayerInfo, MasterPid);

        {game_request_confirmed, OpponentName, MasterPid} ->
            MasterPid ! {new_game_request, Name, OpponentName}, % Notify master process (game.erl)
            schedule_game(Name, Credits, PlayerInfo, MasterPid);

        {game_started, GameId, OpponentName, GameInfo} ->
            Move = rand:uniform(3), % Generate random move (1, 2, or 3)
            MoveName = move_to_string(Move),
            MasterPid ! {move, GameId, Name, Move}, % Send move to master process (game.erl)
            schedule_game(Name, Credits, PlayerInfo, MasterPid);  

        {dq, GameId} ->
            dq_loop(Name, Credits, PlayerInfo, MasterPid)
    end.



    dq_loop(Name, Credits, PlayerInfo, MasterPid) ->
        %No Send function for new requests
        receive
        
        {game_request, _, _} ->
            dq_loop(Name, Credits, PlayerInfo, MasterPid); % Stay in disqualification state

        {game_request_confirmed, _, _} ->
            dq_loop(Name, Credits, PlayerInfo, MasterPid);

        {game_started, GameId, OpponentName, GameInfo} ->
            Move = rand:uniform(3), % Generate random move (1, 2, or 3)
            MoveName = move_to_string(Move),
            MasterPid ! {move, GameId, Name, Move}, % Send move to master process (game.erl)
            dq_loop(Name, Credits, PlayerInfo, MasterPid); % Dont go out of DQ Loop However

        {dq, GameId} ->
            dq_loop(Name, Credits, PlayerInfo, MasterPid)
    end.

send_request(Name, PlayerInfo) ->
    L = length(PlayerInfo),
    RandomIndex = rand:uniform(L),
    {OpponentName, _} = lists:nth(RandomIndex, PlayerInfo),
    OpponentPid = whereis(OpponentName),

    case OpponentName of
        Name ->
            send_request(Name, PlayerInfo);
        _ ->
            case OpponentPid of
                undefined ->
                    io:format("Error: Opponent ~s is not registered~n", [OpponentName]),
                    send_request(Name, PlayerInfo);
                _ ->
                    OpponentPid ! {game_request, Name},
                    % io:format("~s sent game request to ~s~n", [Name, OpponentName]),
                    ok
            end
    end.

move_to_string(1) -> "rock";
move_to_string(2) -> "paper";
move_to_string(3) -> "scissors".
