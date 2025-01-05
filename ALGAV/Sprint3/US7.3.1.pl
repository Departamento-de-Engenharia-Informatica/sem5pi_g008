:-dynamic generations/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.
:-dynamic perc_best_individuals/1.
:-dynamic min_cost/1.
:-dynamic current_best/1.
:-dynamic max_allowed_time/1.
:-dynamic started_at/1.
:- dynamic availability/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_staff2/3.
:-dynamic agenda_operation_room1/3.
:-dynamic assignment_surgery/2.
:-dynamic listasolucoes/1.
:-dynamic roomToUse/1.
:-dynamic dayToUse/1.
:-dynamic surgeryLenght/1.

%another example
agenda_staff(d001,20241028,[]).
agenda_staff(d002,20241028,[]).
agenda_staff(d003,20241028,[]).
agenda_staff(d004,20241028,[]).
agenda_staff(d005,20241028,[]).
agenda_staff(d006,20241028,[]).
agenda_staff(d007,20241028,[]).
agenda_staff(d008,20241028,[]).

timetable(d001,20241028,(1000,1200)).
timetable(d002,20241028,(720,1440)).
timetable(d003,20241028,(600,1320)).
timetable(d004,20241028,(520,1320)).
timetable(d005,20241028,(520,1440)).
timetable(d006,20241028,(520,1440)).
timetable(d007,20241028,(520,1440)).
timetable(d008,20241028,(520,1440)).

%another example
%timetable(d001,20241028,(480,1200)).
%timetable(d002,20241028,(500,1440)).
%timetable(d003,20241028,(520,1320)).
%timetable(d004,20241028,(520,1320)).
%timetable(d005,20241028,(520,1320)).
%timetable(d006,20241028,(520,1320)).

staff(d001,doctor,orthopaedist,[so1,so2,so3,so4]).
staff(d002,doctor,orthopaedist,[so2,so3,so4]).
staff(d003, doctor, anesthetist, [so2, so3]).
staff(d004, doctor, orthopaedist, [so2]).
staff(d005, doctor, cleaner, [so2]).
staff(d006, doctor, cleaner, [so2]).
staff(d007, doctor, anesthetist, [so2, so3]).
staff(d008, doctor, anesthetist, [so2, so3]).

surgery_staff_requirements(operation_team,so2, [(orthopaedist, 2)]).
surgery_staff_requirements(anesthetist_team,so2, [(anesthetist, 2)]).
surgery_staff_requirements(cleaning_team,so2, [(cleaner, 1)]).

surgery_staff_requirements(operation_team,so3, [(orthopaedist, 1)]).
surgery_staff_requirements(anesthetist_team,so3, [(anesthetist, 1)]).
surgery_staff_requirements(cleaning_team,so3, [(cleaner, 1)]).

surgery_staff_requirements(operation_team,so4, [(orthopaedist, 1)]).
surgery_staff_requirements(anesthetist_team,so4, [(anesthetist, 1)]).
surgery_staff_requirements(cleaning_team,so4, [(cleaner, 1)]).

agenda_operation_room(or1,20241028,[(520,579,so100001)]).
agenda_operation_room(ola,20241028,[(0,0,so000)]).

surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).
surgery_id(so100004,so2).
surgery_id(so100005,so3).
surgery_id(so100006,so4).
%surgery_id(so100007,so2).
%surgery_id(so100008,so3).

% parameters initialization
initialize:-
    write('Number of new generations: '),read(NG),
    (retract(generations(_));true), asserta(generations(NG)),

    write('Population size: '),read(PS),
	(retract(population(_));true), asserta(population(PS)),

    write('Probability of crossover (%):'), read(P1),
	PC is P1/100,
	(retract(prob_crossover(_));true),	asserta(prob_crossover(PC)),

    write('Probability of mutation (%):'), read(P2),
	PM is P2/100,
	(retract(prob_mutation(_));true), asserta(prob_mutation(PM)),

    write('Percentage of best individuals to next generation (%):'), read(P3),
    (retract(perc_best_individuals(_));true), asserta(perc_best_individuals(P3)),

    write('Minimum cost automatycly accepted: '), read(MinCost),
    (retract(min_cost(_));true), asserta(min_cost(MinCost)),

    write('Maximum allowed time (in seconds):'), read(MaxTime),
    (retract(max_allowed_time(_));true), asserta(max_allowed_time(MaxTime)),

    write('For which day do you want to schedule the surgeries?'), read(Day),
    (retract(dayToUse(_));true), asserta(dayToUse(Day)).

generate:-
    initialize,

    findall(Surgery, surgery_id(Surgery, _), SurgeryList),

    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff2(D,Day,Agenda))),_),

    findall(Room, agenda_operation_room(Room, _, _), RoomList),
    length(RoomList, NrRooms),

    round_robin(SurgeryList, NrRooms, DistributedSurgeriesList),

    calculate_better_scheduling_per_room(RoomList, NrRooms, DistributedSurgeriesList).

calculate_better_scheduling_per_room(_, 0, _):-!.
calculate_better_scheduling_per_room([RoomToUse | RestRoomsList], NrRooms, [SurgeryList | RestDistributedSurgeriesList]):-

    (retract(started_at(_));true), get_time(StartedAt), asserta(started_at(StartedAt)),
    dayToUse(DayToUse),

    agenda_operation_room(RoomToUse, DayToUse, Agenda),
    retractall(agenda_operation_room1(RoomToUse, DayToUse, _)),
    assertz(agenda_operation_room1(RoomToUse, DayToUse, Agenda)),

    length(SurgeryList, NumT),
    (retract(surgeryLenght(_));true), asserta(surgeryLenght(NumT)),
    (retract(roomToUse(_));true), asserta(roomToUse(RoomToUse)),

    generate_population(SurgeryList, Pop),!,
    nl,write('Room='),write(RoomToUse),nl,write('Pop='),write(Pop),nl,
    evaluate_population(Pop,PopValue),
    write('PopValue='),write(PopValue),nl,
    order_population(PopValue,PopOrd),
    generations(NG),
    generate_generation(0,NG,PopOrd),
    current_best(Best*V),
    
    (
    V = 1441 ->
        write('No feasible solution found.'), nl
    ;
        findall(S,(member(S,Best),schedule_surgery(S,DayToUse,RoomToUse)),_),
        nl,write('Best individual Result: '), write(Best),
        write(' with cost: '),write(V), nl
    ),
    NrRooms1 is NrRooms - 1,
    calculate_better_scheduling_per_room(RestRoomsList, NrRooms1, RestDistributedSurgeriesList).

generate_population(SurgeryList, Pop):-
    population(PopSize),
    surgeryLenght(NumT),
    generate_population(PopSize,SurgeryList,NumT,Pop).

generate_population(0,_,_,[]):-!.
generate_population(PopSize,SurgeryList,NumT,[Ind|Rest]):-
    PopSize1 is PopSize-1,
    generate_population(PopSize1,SurgeryList,NumT,Rest),
    generate_individual(SurgeryList,NumT,Ind),
    not(member(Ind,Rest)).
generate_population(PopSize,SurgeryList,NumT,L):-
    generate_population(PopSize,SurgeryList,NumT,L).

generate_individual([G],1,[G]):-!.

generate_individual(SurgeryList,NumT,[G|Rest]):-
    NumTemp is NumT + 1, % to use with random
    random(1,NumTemp,N),
    remove(N,SurgeryList,G,NewList),
    NumT1 is NumT-1,
    generate_individual(NewList,NumT1,Rest).

remove(1,[G|Rest],G,Rest).
remove(N,[G1|Rest],G,[G1|Rest1]):- N1 is N-1,
            remove(N1,Rest,G,Rest1).


evaluate_population([],[]).
evaluate_population([Ind|Rest],[Ind*V|Rest1]):-
    dayToUse(DayToUse),
    roomToUse(RoomToUse),
     (
         schedule_pending_surgeries(Ind, DayToUse, RoomToUse,Cost) ->
            V is Cost
        ;
            V is 1441
        ),

    evaluate_population(Rest,Rest1).

getTotalSurgeryTime(T, Dur) :-
    surgery(T, Prep, Op, Clean),
    Dur is Prep + Op + Clean.

order_population(PopValue,PopValueOrd):-
    bsort(PopValue,PopValueOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
    bsort(Xs,Zs),
    bchange([X|Zs],Ys).

bchange([X],[X]):-!.

bchange([X*VX,Y*VY|L1],[Y*VY|L2]):-
    VX>VY,!,
    bchange([X*VX|L1],L2).

bchange([X|L1],[X|L2]):-bchange(L1,L2).

generate_generation(G,G,[Best|Pop]):-!,
	write('Generation '), write(G), write(':'), nl, write([Best|Pop]), nl,
    (retract(current_best(_));true), asserta(current_best(Best)).

generate_generation(N,G,[Best|Pop]):-
	write('Generation '), write(N), write(':'), nl, write([Best|Pop]), nl,
    (retract(current_best(_));true), asserta(current_best(Best)),

    (
        evaluate_generationByTime ->
            write('Max allowed time exceeded, stopping generation.'), nl, !;

        \+ evaluate_generationByCost([Best|Pop]) ->
            random_permutation([Best|Pop], PermutedPop),
            crossover(PermutedPop, NPop1),
            mutation(NPop1, NPop),
            evaluate_population(NPop, NPopValue),

            select_method(PermutedPop, NPopValue, PopResult),

            N1 is N + 1,
            generate_generation(N1, G, PopResult)

        ;
        !,
        true
    ).

evaluate_generationByCost(Pop) :-
    (
        min_cost(MinCost),
        Pop = [_*V|_],
        V < MinCost,
        V \= -1
    ) -> true;
    false.

evaluate_generationByTime() :-
    max_allowed_time(MaxTime),
    started_at(StartedAt),
    get_time(Now),
    ElapsedTime is Now - StartedAt,
    ElapsedTime > MaxTime.

select_method(OldGenPop,NewGenPop,Result):-
    append(OldGenPop,NewGenPop, OldAndNewPop),
    list_to_set(OldAndNewPop, OldAndNewPopSet),
	order_population(OldAndNewPopSet,NPopOrd),
    length(OldGenPop, PopSize),
    perc_best_individuals(PercBest),
    NrIndividualsToNextGenPartial is (PopSize * PercBest) // 100,
    (
        NrIndividualsToNextGenPartial < 1 ->
        NrIndividualsToNextGen = 1;
        NrIndividualsToNextGen = NrIndividualsToNextGenPartial
    ),
	remove_n_elements(NrIndividualsToNextGen, NPopOrd, IndividualsToNextGen, RemainingIndividuals),
    NrOfElementsToSelect is PopSize - NrIndividualsToNextGen,
    get_remaining_individuals(NrOfElementsToSelect, RemainingIndividuals, Aux),
    append(IndividualsToNextGen, Aux, NewPop),
    order_population(NewPop, Result).

get_remaining_individuals(N, RemaingIndividuals, Result) :-
    get_remaining_individuals_aux(RemaingIndividuals, ResultUnsorted),
    sort_by_random(ResultUnsorted, ResultBig),
    remove_n_elements(N, ResultBig, Result, _).

get_remaining_individuals_aux([], []) :- !.
get_remaining_individuals_aux([Ind*V|Rest], [Ind*V*R|Rest1]) :-
    random(0.0, 1.0, R),
    get_remaining_individuals_aux(Rest, Rest1).

generate_crossover_points(P1,P2):- generate_crossover_points1(P1,P2).

generate_crossover_points1(P1,P2):-
	surgeryLenght(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).
generate_crossover_points1(P1,P2):-
	generate_crossover_points1(P1,P2).


crossover([ ],[ ]).
crossover([Ind*_],[Ind]).
crossover([Ind1*_,Ind2*_|Rest],[NInd1,NInd2|Rest1]):-
	generate_crossover_points(P1,P2),
	prob_crossover(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
        cross(Ind1,Ind2,P1,P2,NInd1),
	  cross(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	crossover(Rest,Rest1).

cross(Ind1,Ind2,P1,P2,NInd11):-
    sublist(Ind1,P1,P2,Sub1),
	surgeryLenght(NumT),
    R is NumT-P2,
    rotate_right(Ind2,R,Ind21),
    remove(Ind21,Sub1,Sub2),
    P3 is P2 + 1,
    insert(Sub2,Sub1,P3,NInd1),
    removeh(NInd1,NInd11).

fillh([ ],[ ]).

fillh([_|R1],[h|R2]):-
	fillh(R1,R2).

sublist(L1,I1,I2,L):-I1 < I2,!,
    sublist1(L1,I1,I2,L).

sublist(L1,I1,I2,L):-sublist1(L1,I2,I1,L).

sublist1([X|R1],1,1,[X|H]):-!, fillh(R1,H).

sublist1([X|R1],1,N2,[X|R2]):-!,N3 is N2 - 1,
	sublist1(R1,1,N3,R2).

sublist1([_|R1],N1,N2,[h|R2]):-N3 is N1 - 1,
		N4 is N2 - 1,
		sublist1(R1,N3,N4,R2).

rotate_right(L,K,L1):-	surgeryLenght(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):- N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).

remove([],_,[]):-!.

remove([X|R1],L,[X|R2]):- not(member(X,L)),!,
        remove(R1,L,R2).

remove([_|R1],L,R2):-
    remove(R1,L,R2).

insert([],L,_,L):-!.
insert([X|R],L,N,L2):-
    surgeryLenght(T),
    ((N>T,!,N1 is N mod T);N1 = N),
    insert1(X,N1,L,L1),
    N2 is N + 1,
    insert(R,L1,N2,L2).


insert1(X,1,L,[X|L]):-!.
insert1(X,N,[Y|L],[Y|L1]):-
    N1 is N-1,
    insert1(X,N1,L,L1).


removeh([],[]).

removeh([h|R1],R2):-!,
    removeh(R1,R2).

removeh([X|R1],[X|R2]):-
    removeh(R1,R2).

mutation([],[]).
mutation([Ind|Rest],[NInd|Rest1]):-
	prob_mutation(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutation(Rest,Rest1).

mutacao1(Ind,NInd):-
	generate_crossover_points(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).

remove_n_elements(N, List, Removeds, Result) :-
    length(Prefix, N),
    append(Prefix, Result, List),
    Removeds = Prefix.

sort_by_random(List, SortedList) :-
    predsort(compare_by_r, List, TempSortedList),
    remove_random_values(TempSortedList, SortedList),!.

remove_random_values([], []).
remove_random_values([A*B*_|Rest], [A*B|Rest1]) :-
    remove_random_values(Rest, Rest1).

compare_by_r(>, _*_*R1, _*_*R2) :-
    R1 > R2.
compare_by_r(<, _*_*R1, _*_*R2) :-
    R1 < R2.
compare_by_r(=, _*_*R1, _*_*R2) :-
    R1 =:= R2.

assignment_surgery(so100001,d001).

find_free_agendas(Date):-
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Date,L),free_agenda_staff0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_).

free_agenda_staff0([],[(0,1440)]).
free_agenda_staff0([(0,Tfin,_)|LT],LT1):-!,free_agenda_staff1([(0,Tfin,_)|LT],LT1).
free_agenda_staff0([(Tin,Tfin,_)|LT],[(0,T1)|LT1]):- T1 is Tin-1,
    free_agenda_staff1([(Tin,Tfin,_)|LT],LT1).

free_agenda_staff1([(_,Tfin,_)],[(T1,1440)]):-Tfin\==1440,!,T1 is Tfin+1.
free_agenda_staff1([(_,_,_)],[]).
free_agenda_staff1([(_,T,_),(T1,Tfin2,_)|LT],LT1):-Tx is T+1,T1==Tx,!,
    free_agenda_staff1([(T1,Tfin2,_)|LT],LT1).
free_agenda_staff1([(_,Tfin1,_),(Tin2,Tfin2,_)|LT],[(T1,T2)|LT1]):-T1 is Tfin1+1,T2 is Tin2-1,
    free_agenda_staff1([(Tin2,Tfin2,_)|LT],LT1).

adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).

intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

intersect_2_agendas([],_,[]).
intersect_2_agendas([D|LD],LA,LIT):-
    intersect_availability(D,LA,LI,LA1),
    intersect_2_agendas(LD,LA1,LID),
	append(LI,LID,LIT).

intersect_availability((_,_),[],[],[]).

intersect_availability((_,Fim),[(Ini1,Fim1)|LD],[],[(Ini1,Fim1)|LD]):-
		Fim<Ini1,!.

intersect_availability((Ini,Fim),[(_,Fim1)|LD],LI,LA):-
		Ini>Fim1,!,
		intersect_availability((Ini,Fim),LD,LI,LA).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)],[(Fim,Fim1)|LD]):-
		Fim1>Fim,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)|LI],LA):-
		Fim>=Fim1,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_),
		intersect_availability((Fim1,Fim),LD,LI,LA).

min_max(I,I1,I,I1):- I<I1,!.
min_max(I,I1,I1,I).

schedule_pending_surgeries([], Date, Room,Cost) :-
    agenda_operation_room(Room, Date, Agenda),
    last(Agenda, (_, Fim, _)),
    Cost is Fim,
    agenda_operation_room1(Room, Date, Agenda1),
    retract_data(Date,Room).

schedule_pending_surgeries([X | Rest], Date, Room,Cost) :-
    (
        schedule_surgery(X, Date, Room) ->
        true
    ;
        retract_data(Date,Room),
        fail
    ),
    schedule_pending_surgeries(Rest, Date, Room,Cost).

retract_data(Date,Room):-
    retractall(agenda_staff(D,Dia,_)),
    findall(_,(agenda_staff2(D,Day,Agenda),assertz(agenda_staff(D,Day,Agenda))),_),
    agenda_operation_room1(Room, Date, Agenda),
    retractall(agenda_operation_room(Room, Date, _)),
    assertz(agenda_operation_room(Room, Date, Agenda)).


schedule_surgery(Surgery, Date, Room) :-
    surgery_id(Surgery, Tipo),
    surgery(Tipo,Time_Anesthesia,Time_Surgery,Time_Cleaning),
    Duration is Time_Anesthesia + Time_Surgery + Time_Cleaning,
    surgery_staff_requirements(operation_team,Tipo, Requirements),
    surgery_staff_requirements(anesthetist_team,Tipo,Requirements2),
    surgery_staff_requirements(cleaning_team,Tipo, Requirements3),
    find_free_agendas(Date),
    get_staff_for_surgery(Requirements, StaffList),
    get_staff_for_surgery(Requirements2, Staff_AnesthesiaList),
    get_staff_for_surgery(Requirements3, Staff_CleaningList),
    findall(CommonIntervals, (member(Team, StaffList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals),
    findall(CommonIntervals, (member(Team, Staff_AnesthesiaList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals_Anesthesia),
    findall(CommonIntervals, (member(Team, Staff_CleaningList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals_Cleaning),
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals), select_sufficient_interval(CommonIntervals, Time_Surgery, SurgeryInterval)), AllSurgeryInterval),
    flatten(AllSurgeryInterval, Result_surgery),
    list_to_set(Result_surgery, UniqueResultsurgery),
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals_Anesthesia), select_sufficient_interval(CommonIntervals, Time_Anesthesia, SurgeryInterval)), AllSurgeryInterval_Anesthesia),
    flatten(AllSurgeryInterval_Anesthesia, Result_Anesthesia),
    list_to_set(Result_Anesthesia, UniqueResultAnesthesia),
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals_Cleaning), select_sufficient_interval(CommonIntervals, Time_Cleaning, SurgeryInterval)), AllSurgeryInterval_Cleaning),
    flatten(AllSurgeryInterval_Cleaning, Result_Cleaning),
    list_to_set(Result_Cleaning, UniqueResultCleaning),
    precede(UniqueResultAnesthesia,UniqueResultsurgery,UniqueResultCleaning,Result_allCombinatios),
    list_to_set(Result_allCombinatios, Result_allCombinatios1),
    agenda_operation_room(Room, Date, RoomAgenda),
    findall(NewInterval, (
    member(SurgeryInterval1, Result_allCombinatios),
    set_new_interval(SurgeryInterval1,RoomAgenda,Duration,NewInterval)), StaffRoomIntervals),
    flatten(StaffRoomIntervals, StaffRoomIntervals1),
    list_to_set(StaffRoomIntervals1, StaffRoomIntervals2),
    findall(SurgeryInterval, (member(SurgeryInterval, StaffRoomIntervals2), check_room_availability(Room, Date, SurgeryInterval)), ValidRoomIntervals),
    min_final_minute(ValidRoomIntervals, MinInterval, UpdatedList),
    assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,ValidRoomIntervals,Staff_CleaningList,StaffList).


       assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,ValidRoomIntervals,Staff_CleaningList,StaffList):-
(      assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,Staff_CleaningList,StaffList)
    -> true
    ;
        min_final_minute(ValidRoomIntervals, MinInterval1, UpdatedList),
        assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval1,Staff_AnesthesiaList,UpdatedList,Staff_CleaningList,StaffList)
).

assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,Staff_CleaningList,StaffList) :-

    find_staff_with_min_interval(StaffList, Date, MinInterval, StaffWithMinInterval, Tipo),
    find_staff_with_min_intervalAnesthesia(Staff_AnesthesiaList, Date, MinInterval, StaffWithMinInterval_AnesthesiaList, Tipo),
    find_staff_with_min_intervalClenaing(Staff_CleaningList, Date, MinInterval, StaffWithMinInterval_CleaningLis, Tipo),
    [SelectedTeam | _] = StaffWithMinInterval,
    [SelectedTeamAnesthesia | _] = StaffWithMinInterval_AnesthesiaList,
    [SelectedTeamCleaning | _] = StaffWithMinInterval_CleaningLis,
    add_assignment_surgery(Surgery, Room),
    (Start, End) = MinInterval,
    AdjustedStartSurgery is Start + Time_Anesthesia,
    AdjustedEndSurgery is End - Time_Cleaning,
    AdjustedStartAnesthesia is Start,
    AdjustedEndAnesthesia is End - Time_Cleaning,
    AdjustedStartCleaning is Start + Time_Anesthesia + Time_Surgery,
    AdjustedEndCleaning is End,
    update_staff_agendas(SelectedTeam, Date, (AdjustedStartSurgery, AdjustedEndSurgery), Surgery),
    update_staff_agendas(SelectedTeamAnesthesia, Date, (AdjustedStartAnesthesia, AdjustedEndAnesthesia), Surgery),
    update_staff_agendas(SelectedTeamCleaning, Date, (AdjustedStartCleaning, AdjustedEndCleaning), Surgery),
    update_room_agenda(Room, Date, MinInterval, Surgery).

assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo) :-
    assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo).

get_staff_for_surgery([], [[]]).
get_staff_for_surgery([(Type, Quantity) | RestRequirements], AllStaffLists) :-
    findall(ID, staff(ID, _, Type, _), AllStaff),
    findall(Selected, (combination(Quantity, AllStaff, Selected)), PossibleSelections),
    get_staff_for_surgery(RestRequirements, RestStaffLists),
    findall(AppendedList,
        (member(Selected, PossibleSelections),
         member(RestStaff, RestStaffLists),
         append(Selected, RestStaff, AppendedList)),
         AllStaffLists).

combination(0, _, []).
combination(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    combination(N1, T, R).
combination(N, [_|T], R) :-
    N > 0,
    combination(N, T, R).


select_sufficient_interval([(Start, End) | Rest], Duration, (Start, End)) :-
    End - Start + 1 >= Duration,
    (Rest = [] ;
    \+ (member((Start2, End2), Rest),
    End2 - Start2 + 1 >= Duration,
    Start2 < Start)).

select_sufficient_interval([_ | Rest], Duration, Interval) :-
    select_sufficient_interval(Rest, Duration, Interval).

check_room_availability(Room, Date, (Start, End)) :-
    agenda_operation_room(Room, Date, RoomAgenda),
findall((AStart, AEnd, _), (
    member((AStart, AEnd, _), RoomAgenda),
    Start < AEnd,
    End > AStart
    ), Conflicts),
    Conflicts = [].

update_staff_agendas([], _, _, _).
update_staff_agendas([ID | Rest], Date, (Start, End), Surgery) :-
    agenda_staff(ID, Date, CurrentAgenda),

    append(CurrentAgenda, [(Start, End, Surgery)], UpdatedAgenda),

    retractall(agenda_staff(ID, Date, _)),
    assertz(agenda_staff(ID, Date, UpdatedAgenda)),

    update_staff_agendas(Rest, Date, (Start, End), Surgery).

update_room_agenda(Room, Date, (Start, End), Surgery) :-
    agenda_operation_room(Room, Date, CurrentAgenda),
    append(CurrentAgenda, [(Start, End, Surgery)], UpdatedAgenda),
    retractall(agenda_operation_room(Room, Date, _)),
    assertz(agenda_operation_room(Room, Date, UpdatedAgenda)).

add_assignment_surgery(SurgeryID, Room) :-
    assertz(assignment_surgery(SurgeryID, Room)).

min_final_minute(AllCommonIntervals, MinInterval,NewList_Unique) :-
    flatten(AllCommonIntervals, Flattened),
    findall((Start, End), member((Start, End), Flattened), Intervals),
    findall(End, member((_, End), Intervals), EndMinutes),
    min_list(EndMinutes, MinMinute), % menor minuto final
    member(MinInterval1, Intervals),
    MinInterval1 = (_, MinMinute),
    select(MinInterval1, AllCommonIntervals, NewList_Unique),
     select(MinInterval, AllCommonIntervals, NewList_Unique).

find_staff_with_min_interval(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
    findall(Team, (
        member(Team, StaffList),
        intersect_all_agendas(Team, Date, CommonIntervals),
        is_available_in_intervals(MinInterval, CommonIntervals,Tipo)
    ), StaffWithMinInterval).

 find_staff_with_min_intervalClenaing(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
     findall(Team, (
         member(Team, StaffList),
         intersect_all_agendas(Team, Date, CommonIntervals),
         is_available_in_intervalsClenaing(MinInterval, CommonIntervals,Tipo)
     ), StaffWithMinInterval).

find_staff_with_min_intervalAnesthesia(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
    findall(Team, (
        member(Team, StaffList),
        intersect_all_agendas(Team, Date, CommonIntervals),
        is_available_in_intervalsAnesthesia(MinInterval, CommonIntervals,Tipo)
    ), StaffWithMinInterval).

is_available_in_intervals((Start, End), CommonIntervals,Tipo) :-
    surgery(Tipo,Time_Anesthesia,_,Time_Cleaning),
    member((CStart, CEnd), CommonIntervals),
    CStart =< Start + Time_Anesthesia,
    CEnd >= End - Time_Cleaning.

is_available_in_intervalsClenaing((Start, End), CommonIntervals,Tipo) :-
    surgery(Tipo,Time_Anesthesia,Time_Surgery,_),
    member((CStart, CEnd), CommonIntervals),
    CStart =< Start +Time_Surgery+Time_Anesthesia,
    CEnd >= End.

is_available_in_intervalsAnesthesia((Start, End), CommonIntervals,Tipo) :-
    surgery(Tipo,Time_Anesthesia,Time_Surgery,_),
    member((CStart, CEnd), CommonIntervals),
    CStart =< Start+Time_Anesthesia+Time_Surgery,
    CEnd >= End.

set_new_interval([(I1, _), (_, _), (_, F3)], RoomAgenda, Duration, NewIntervals) :-
    % Encontra todos os intervalos válidos na agenda
    findall((AEndAuxIncrement, AuxIncrement), (
        member((_, AEnd, _), RoomAgenda),
        I1 =< AEnd,
        Aux is AEnd + 1 + Duration,
        AEndAux is AEnd + 1,
        Aux =< F3,
        between(0, 1440, Step),
        AEndAuxIncrement is AEndAux + Step,
        AuxIncrement is Aux + Step
    ), IntervalsFromEnd),
    findall((AEndAuxIncrement, AuxIncrement), (
        member((_, AEnd, _), RoomAgenda),
        I1 > AEnd,
        Aux is I1 + Duration,
        Aux =< F3,
        between(0, 1440, Step),
        AEndAuxIncrement is I1 + Step,
        AuxIncrement is Aux + Step
        ), IntervalsFromEnd2),
    findall((AEndAuxIncrement, AuxIncrement), (
        member((AStart, _, _), RoomAgenda),
        I1 =< AStart,
        Aux is I1 + Duration,
        Aux =< F3,
        between(0, 1440, Step),
        AEndAuxIncrement is I1 + Step,
        AuxIncrement is Aux + Step
        ), IntervalsFromStart) ,
    append(IntervalsFromEnd, IntervalsFromEnd2, NewIntervals0),
    append(NewIntervals0, IntervalsFromStart, NewIntervals).

precede([], [], [], []).

% Gera combinações válidas
precede(L1, L2, L3, Result) :-
    findall([X, Y, Z], (
        member(X, L1),
        member(Y, L2),
        member(Z, L3),
        X = (I1, F1),
        Y = (I2, F2),
        Z = (I3, _),
        I1 =< I2,
        F1 >= I2,
        I2 =< I3,
        F2 >= I3
       ), Result).


round_robin(Input, N, Result) :-
    length(Buckets, N),
    maplist(=([]), Buckets),
    distribute(Input, Buckets, 1, N, Result).

distribute([], Buckets, _, _, Buckets).
distribute([H|T], Buckets, Index, N, Result) :-
    nth1(Index, Buckets, Bucket, RestBuckets),
    append(Bucket, [H], UpdatedBucket),
    nth1(Index, UpdatedBuckets, UpdatedBucket, RestBuckets),
    NextIndex is (Index mod N) + 1,
    distribute(T, UpdatedBuckets, NextIndex, N, Result).
