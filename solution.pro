% basak tepe
% 2020400117
% compiling: yes
% complete: yes


distance(0, 0, 0).  % a dummy predicate to make the sim work.

% PREDICATE 1
%distance(+Agent, +TargetAgent, -Distance).

distance(Agent, TargetAgent, Distance):-
    Distance is abs( Agent.x - TargetAgent.x) + abs(Agent.y - TargetAgent.y).


% PREDICATE 2
% multiverse_distance(+StateId, +AgentId, +TargetStateId, +TargetAgentId, -Distance).

%we needed extra predicates.
get_universe_id(StateId, UniverseId) :- history(StateId, UniverseId, _, _).
get_time(StateId, Time) :- history(StateId, _, Time, _).
get_agents(StateId,Agents) :- state(StateId, Agents, _, _).

multiverse_distance(0,0,0,0,0). %dummy


multiverse_distance(StateId, AgentId, TargetStateId, TargetAgentId, Distance) :-
    %TravelCost is (Class = wizard -> 2 ; 5),   
    get_agents(StateId,Agents), 
    get_agents(TargetStateId,TargetAgents), 
    (Agents.get(AgentId).class = wizard -> TravelCost = 2; TravelCost = 5),

    get_time(StateId, Time),
    get_time(TargetStateId, TargetTime),
    get_universe_id(StateId, UniverseId),
    get_universe_id(TargetStateId, TargetUniverseId),

    %travel cost is variant.

    Distance is abs(Agents.get(AgentId).x - TargetAgents.get(TargetAgentId).x) +  
                abs(Agents.get(AgentId).y - TargetAgents.get(TargetAgentId).y) +
              TravelCost * (abs(Time - TargetTime) + abs(UniverseId - TargetUniverseId)).


%PREDICATE 3
%extra predicate
get_first_element([K-V|_], K-V).

%nearest agent(+StateId, +AgentId, -NearestAgentId, -Distance)

evaluate_all_distances(Agents, AgentId, NearestAgentId, ShortestDistance) :-
    findall(
        CurrentDistance-NextAgentId,
        (
            % Get the reference and next agents
            ReferenceAgent = Agents.get(AgentId),
            NextAgent = Agents.get(NextAgentId),

            % Calculate the distance between the reference and next agents
            distance(ReferenceAgent, NextAgent, CurrentDistance),

            % Make sure the next agent is not the reference agent and the names are also different
            NextAgentId \= AgentId,
            ReferenceAgent.name \=  NextAgent.name
        ),
        Distances
    ),
    % Sort the distances in ascending order
    keysort(Distances, SortedDistances),
    % Get the nearest agent id and distance
    length(SortedDistances, Length),
    (Length>0 -> get_first_element(SortedDistances, ShortestDistance-NearestAgentId); NearestAgentId = 0, ShortestDistance = 0).


nearest_agent(0, 0, -999, -999).

nearest_agent(StateId, AgentId, NearestAgentId, Distance) :-
    state(StateId, Agents, _, TurnOrder),
    length(TurnOrder, NumAgents),

    %agent is alone, there is no nearest agent.
    (NumAgents =< 1 -> 
        NearestAgentId = -999,
        Distance = -999
    ;
        NumAgents > 1,
        evaluate_all_distances(Agents, AgentId, NearestAgentId, Distance)
    ).



%PREDICATE 4
% nearest_agent_in_multiverse(StateId, AgentId, TargetStateId, TargetAgentId, Distance).
%similar to 3rd predicate but we have to do 2 findalls.
%1 to traverse all universes and 1 to traverse all states (agent lists)


evaluate_all_multiuniversal_distances(StateId, AgentId, NearestStateId, NearestAgentId, Distance) :-
    get_agents(StateId, Agents),

    findall(
        CurrentDistance-TargetAgentId-TargetStateId,
        (
            % Get the reference and next agents
            ReferenceAgent = Agents.get(AgentId),
            TargetAgent = Agents.get(TargetAgentId),

            % Calculate the distance between the reference and next agents
            % multiverse_distance(+StateId, +AgentId, +TargetStateId, +TargetAgentId, -Distance).
            multiverse_distance(StateId, AgentId, TargetStateId,TargetAgentId, CurrentDistance),

            % Make sure the next agent is not the reference agent and the names are also different
            TargetAgentId \= AgentId,
            ReferenceAgent.name \=  TargetAgent.name
        ),
        Distances
    ),
    % Sort the distances in ascending order
    keysort(Distances, SortedDistances),
    % Get the nearest agent id and distance
    SortedDistances = [NewDistance-NewNearestAgentId-NewNearestStateId|_],
    Distance is NewDistance,
    NearestAgentId is NewNearestAgentId,
    NearestStateId is NewNearestStateId.



nearest_agent_in_multiverse(0, 0, 0, 0, 0).

%nearest agent in multiverse(+StateId, +AgentId, -TargetStateId, -TargetAgentId, -Distance)
nearest_agent_in_multiverse(StateId, AgentId, NearestStateId, NearestAgentId, Distance) :-
    %i did not consider a case where there is only 1 agent in the multiverse, it might be possible but the game would not make sense.
    evaluate_all_multiuniversal_distances(StateId, AgentId, NearestStateId, NearestAgentId, Distance).



%PREDICATE 5
% num_agents_in_state(+StateId, +Name, -NumWarriors, -NumWizards, -NumRogues).

num_agents_in_state(0, _, 0, 0, 0).

num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues) :-
    get_agents(StateId,Agents),

    findall(
        Agent, 
        (
            Agent = Agents.get(AgentId),
            Agent.name \= Name,    
            Agent.class == rogue
        ),
        Rogues
    ),

    findall(
        Agent, 
        (
            Agent = Agents.get(AgentId),
            Agent.name \= Name,    
            Agent.class == wizard
        ),
        Wizards
    ),

    findall(
        Agent, 
        (
            Agent = Agents.get(AgentId),
            Agent.name \= Name,    
            Agent.class == warrior
        ),
        Warriors
    ),

    length(Rogues,NumRogues),
    length(Warriors,NumWarriors),
    length(Wizards,NumWizards).





%PREDICATE 6
% difficulty_of_state(+StateId, +Name, +AgentClass, -Difficulty).

difficulty_of_state(0, _, 0, 0) :-!.

difficulty_of_state(StateId, Name, AgentClass, Difficulty) :-

    num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues),

    (AgentClass == warrior -> 
        Difficulty is 5 * NumWarriors + 8 * NumWizards + 2 * NumRogues
    ;
    AgentClass == wizard ->
        Difficulty is 2 * NumWarriors + 5 * NumWizards + 8 * NumRogues
    ;
    AgentClass == rogue ->
        Difficulty is 8 * NumWarriors + 2 * NumWizards + 5 * NumRogues
    ).


%PREDICATE 7
% easiest_traversable_state(+StateId, +AgentId, -TargetStateId).



%extra predicates
find_all_portal_traversable_states(0, 0, []).

find_all_portal_traversable_states(StateId, AgentId, PortalTraversableStates) :-
    state(StateId, Agents, CurrentTurn, TurnOrder),
    Agent = Agents.get(AgentId),

    findall(PortalTargetStateId,
    %a state is traversable if it can be reached by portal action.
    (       can_perform(Agent.class, portal),
            history(StateId, UniverseId, Time, _),
            state(StateId, Agents, CurrentTurn, TurnOrder),
            % Agent = Agents.get(AgentId),
            % check whether global universe limit has been reached
            global_universe_id(GlobalUniverseId),
            universe_limit(UniverseLimit),
            GlobalUniverseId < UniverseLimit,
            % agent cannot time travel if there is only one agent in the universe
            length(TurnOrder, NumAgents),
            NumAgents > 1,
            history(_, TargetUniverseId, TargetTime, _),
            % check whether target is now or in the past
            current_time(TargetUniverseId, TargetUniCurrentTime, _),
            TargetTime < TargetUniCurrentTime,
            % check whether there is enough mana
            (Agent.class = wizard -> TravelCost = 2; TravelCost = 5),
            Cost is abs(TargetTime - Time)*TravelCost + abs(TargetUniverseId - UniverseId)*TravelCost,
            Agent.mana >= Cost,
            % check whether the target location is occupied
            get_earliest_target_state(TargetUniverseId, TargetTime, PortalTargetStateId),
            state(PortalTargetStateId, TargetAgents, _, TargetTurnOrder),
            TargetState = state(PortalTargetStateId, TargetAgents, _, TargetTurnOrder),
            \+tile_occupied(Agent.x, Agent.y, TargetState),
            difficulty_of_state(PortalTargetStateId, Agent.name, Agent.class, Difficulty),
            % The difficulty of the easiest traversable state should be greater than zero. 
            Difficulty>0
        ),
        PortalTraversableStates).



find_all_portal_to_now_traversable_states(0, 0, []).

find_all_portal_to_now_traversable_states(StateId, AgentId, PortalToNowTraversableStates) :-
    state(StateId, Agents, CurrentTurn, TurnOrder),
    Agent = Agents.get(AgentId),

    findall(PortalToNowTargetStateId,
        %a state is traversable if it can be reached by portal to now action.
        (   can_perform(Agent.class, portal_to_now), 
            % get_current_agent_and_state(UniverseId, AgentId, StateId),
            history(StateId, UniverseId, Time, _),
            state(StateId, Agents, CurrentTurn, TurnOrder),
            Agent = Agents.get(AgentId),
            % agent cannot time travel if there is only one agent in the universe
            length(TurnOrder, NumAgents),
            NumAgents > 1,
            % agent can only travel to now if it's the first turn in the target universe
            current_time(TargetUniverseId, TargetTime, 0),
            % agent cannot travel to current universe's now (would be a no-op)
            \+(TargetUniverseId = UniverseId),
            % check whether there is enough mana
            (Agent.class = wizard -> TravelCost = 2; TravelCost = 5),
            Cost is abs(TargetTime - Time)*TravelCost + abs(TargetUniverseId - UniverseId)*TravelCost,
            Agent.mana >= Cost,
            % check whether the target location is occupied
            get_latest_target_state(TargetUniverseId, TargetTime, PortalToNowTargetStateId),
            state(PortalToNowTargetStateId, TargetAgents, _, TargetTurnOrder),
            TargetState = state(PortalToNowTargetStateId, TargetAgents, _, TargetTurnOrder),
            \+tile_occupied(Agent.x, Agent.y, TargetState),
            difficulty_of_state(PortalToNowTargetStateId, Agent.name, Agent.class, Difficulty),
            % The difficulty of the easiest traversable state should be greater than zero. 
            Difficulty>0
        ),
        PortalToNowTraversableStates).


find_all_traversable_states(0, 0, []).

find_all_traversable_states(StateId, AgentId, TraversableStates) :-
        state(StateId, Agents, _, _),
        Agent = Agents.get(AgentId),

        %find all states that can be performed portal or portal-to-now 
        find_all_portal_traversable_states(StateId, AgentId, PortalTraversableStates),
        find_all_portal_to_now_traversable_states(StateId, AgentId, PortalToNowTraversableStates),
        %merge these lists
        append(PortalTraversableStates, PortalToNowTraversableStates, TraversableStatesWithoutCurrent),
        %current state is also traversable, we add the current state to the list
        difficulty_of_state(StateId, Agent.name, Agent.class, CurrentStateDifficulty),
        (CurrentStateDifficulty>0 -> TraversableStates = [StateId | TraversableStatesWithoutCurrent]; TraversableStates = TraversableStatesWithoutCurrent).
        



easiest_traversable_state(0, 0, 0):- !.

easiest_traversable_state(StateId, AgentId, TargetStateId) :- 
        state(StateId, Agents, _, _),
        Agent = Agents.get(AgentId),

    findall(
        Difficulty-PossibleStateId, 
        ((
            %calculate difficulty and make a pairwise list for each traversable state id
            find_all_traversable_states(StateId, AgentId, TraversableStates),
            member(PossibleStateId,TraversableStates),
            difficulty_of_state(PossibleStateId, Agent.name, Agent.class, Difficulty),
            % The difficulty of the easiest traversable state should be greater than zero. 
            Difficulty > 0
        )),
        StateDifficulties
        ),

        keysort(StateDifficulties, SortedStateDifficulties),
        SortedStateDifficulties = [Difficulty-TargetStateId|_].


%PREDICATE 8

%basic action policy(+StateId, +AgentId, -Action)
basic_action_policy(0, 0, _):-!.

basic_action_policy(StateId, AgentId, Action) :-

    state(StateId, Agents, _, _),
    Agent = Agents.get(AgentId),
    nearest_agent(StateId, AgentId, NearestAgentId, Distance),

    
    
    %1. The agent should try to portal to the easiest traversable state if possible  (if allowed)
    ((( 
        find_all_portal_traversable_states(StateId, AgentId, Portalables),
        easiest_traversable_state(StateId, AgentId, EasiestTargetStateId),
        length(Portalables, PortalableCount),
        %list should not be empty
        PortalableCount =\= 0,

        %we want to portal to the easiest traversable state, i.e easiest state should be among portalable states
        member(EasiestTargetStateId,Portalables),

        history(EasiestTargetStateId, UniverseId, _, _),
        Action = [portal,UniverseId]);

    (   
        find_all_portal_to_now_traversable_states(StateId, AgentId, PortalToNowables),
        easiest_traversable_state(StateId, AgentId, EasiestTargetStateId),
        length(PortalToNowables, PortalToNowableCount),
        list should not be empty
        PortalToNowableCount =\= 0,
        

        %we want to portal to now to the easiest traversable state, i.e easiest state should be among portaltonowable states
        member(EasiestTargetStateId,PortalToNowables),

        history(EasiestTargetStateId, UniverseId, _, _),
        Action = [portal_to_now,UniverseId]));

    %4. Resting is the last option, however if there is no nearest agent, the agent cannot perform attack or move so it should rest.
    %-999 corrresponds to the dummy agent id we return from nearest_agent when the agent is alone in the state.
    (NearestAgentId =:= -999 -> 
    Action = [rest]);


    %2. If it cannot travel to the easiest traversable state, it should approach to the nearest different
    %right up left down agent until it is in the attack range of the nearest agent.
    %there has to be a nearest agent to do so.

    (((Distance> 1, Agent.class = warrior ->
    (Agent.x <  Agents.get(NearestAgentId).x -> Action = [move_right];
    Agent.y <  Agents.get(NearestAgentId).y -> Action = [move_up];
    Agent.x >  Agents.get(NearestAgentId).x -> Action = [move_left];
    Agent.y >  Agents.get(NearestAgentId).y -> Action = [move_down]
    ));
    (Distance> 5, Agent.class = rogue ->
    (Agent.x <  Agents.get(NearestAgentId).x -> Action = [move_right];
    Agent.y <  Agents.get(NearestAgentId).y -> Action = [move_up];
    Agent.x >  Agents.get(NearestAgentId).x -> Action = [move_left];
    Agent.y >  Agents.get(NearestAgentId).y -> Action = [move_down]
    ));
    (Distance> 10,  Agent.class = wizard ->
    (Agent.x <  Agents.get(NearestAgentId).x -> Action = [move_right];
    Agent.y <  Agents.get(NearestAgentId).y -> Action = [move_up];
    Agent.x >  Agents.get(NearestAgentId).x -> Action = [move_left];
    Agent.y >  Agents.get(NearestAgentId).y -> Action = [move_down]
    ))));


    %3. Once the agent is in the attack range of the nearest agent, it should attack the nearest agent.
    ((Distance=<1, Agent.class = warrior-> Action = [melee_attack, NearestAgentId]);
    (Distance=<5,  Agent.class = rogue -> Action = [ranged_attack, NearestAgentId]);
    (Distance=<10, Agent.class = wizard -> Action = [magic_missile,  NearestAgentId]))).

