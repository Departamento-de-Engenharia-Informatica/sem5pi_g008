:- use_module(library(http/http_open)).
:- use_module(library(http/json)).



:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.

:- dynamic surgery/4.
:- dynamic surgery_id/2.
:- dynamic assignment_surgery/2.
:- dynamic staff/4.
:- dynamic timetable/3.
:- dynamic agenda_staff/3.



% Fetch JSON from a backend URL
fetch_json(URL, JSON) :-
    setup_call_cleanup(
        http_open(URL, In, []),
        json_read_dict(In, JSON),
        close(In)
    ).


% Clear all data including agenda_staff
clear_all_data :- 
    clear_surgeries_data,
    clear_staff_data,
    clear_timetable_data,
    clear_surgery_id_data,
    clear_agenda_staff_data,
    clear_agenda_operation_room_data,
    clear_assignment_surgery_data.



% Load all data including agenda_staff
load_data :- 
    load_surgeries_data,
    load_staff_data,
    load_timetable_data,
    load_surgeryId_data,
    load_agenda_staff_data,
    load_agenda_operation_room_data,
    load_assignment_surgery_data.
   


% Convert time in "HH:MM:SS" format to minutes (including seconds)
time_to_minutes(TimeString, Minutes) :-
    split_string(TimeString, ":", "", [HStr, MStr, SStr]),
    number_string(Hours, HStr),
    number_string(MinutesPart, MStr),
    number_string(Seconds, SStr),
    TotalMinutes is Hours * 60 + MinutesPart + (Seconds / 60),
    Minutes is round(TotalMinutes).  % Round to the nearest minute

% Convert String to Atom
string_to_atom_safe(String, Atom) :-
    (var(String) -> Atom = 'Unknown'; string_to_atom(String, Atom)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_surgeries_data :-
        fetch_json('https://localhost:7081/api/Planning/get-surgeries', SurgeriesJSON),
        process_surgeries(SurgeriesJSON).

% Process surgeries JSON and assert them as facts
process_surgeries(SurgeriesJSON) :-
    maplist(assert_surgery, SurgeriesJSON),
    maplist(print_surgery, SurgeriesJSON).

% Assert a surgery fact
assert_surgery(Surgery) :-
    (time_to_minutes(Surgery.anesthesia, AnesthesiaMinutes) -> true ; AnesthesiaMinutes = 0),
    (time_to_minutes(Surgery.surgery, SurgeryMinutes) -> true ; SurgeryMinutes = 0),
    (time_to_minutes(Surgery.cleaning, CleaningMinutes) -> true ; CleaningMinutes = 0),
    string_to_atom_safe(Surgery.opTypeCode, OpTypeCodeAtom),
    assertz(surgery(OpTypeCodeAtom, AnesthesiaMinutes, SurgeryMinutes, CleaningMinutes)).


% Print surgery details to the console
print_surgery(Surgery) :-
    (time_to_minutes(Surgery.anesthesia, AnesthesiaMinutes) -> true ; AnesthesiaMinutes = 0),
    (time_to_minutes(Surgery.surgery, SurgeryMinutes) -> true ; SurgeryMinutes = 0),
    (time_to_minutes(Surgery.cleaning, CleaningMinutes) -> true ; CleaningMinutes = 0),
    format('Surgery ID: ~w\n', [Surgery.opTypeCode]),
    format('  Anesthesia Time: ~w minutes\n', [AnesthesiaMinutes]),
    format('  Surgery Time: ~w minutes\n', [SurgeryMinutes]),
    format('  Cleaning Time: ~w minutes\n', [CleaningMinutes]), nl.


% Clear all surgery data from the knowledge base
clear_surgeries_data :-
    retractall(surgery(_, _, _, _)),
    write('All surgery data cleared from the knowledge base.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load staff data from the backend
load_staff_data :-
    fetch_json('https://localhost:7081/api/Planning/get-staff', StaffsJSON),
    process_staffs(StaffsJSON).

% Process staff JSON and assert them as facts
process_staffs(StaffsJSON) :-
    maplist(assert_staff, StaffsJSON),
    maplist(print_staff, StaffsJSON).


% Assert a staff fact
assert_staff(Staff) :-
    % Garantir que valores nulos sejam tratados corretamente
    string_to_atom_safe(Staff.get(licenseNumber, null), LicenseNumber),
    string_to_atom_safe(Staff.get(staffType, null), StaffType),
    string_to_atom_safe(Staff.get(specialization, null), Specialization),
    SurgeryTypes = Staff.get(operationTypeCodes, []), %% VER MELHOR 
    assertz(staff(LicenseNumber, StaffType, Specialization, SurgeryTypes)).

% Print staff details to the console
print_staff(Staff) :-
    string_to_atom_safe(Staff.get(licenseNumber, null), LicenseNumber),
    string_to_atom_safe(Staff.get(staffType, null), StaffType),
    string_to_atom_safe(Staff.get(specialization, null), Specialization),
    OperationTypeCodes = Staff.get(operationTypeCodes, []),
    format('Staff License Number: ~w\n', [LicenseNumber]),
    format('  Staff Type: ~w\n', [StaffType]),
    format('  Specialization: ~w\n', [Specialization]),
    format('  Qualified Operation Types: ~w\n', [OperationTypeCodes]), nl.

% Clear all staff data from the knowledge base
clear_staff_data :-
    retractall(staff(_, _, _, _)),
    write('All staff data cleared from the knowledge base.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%´

% Load timetable data from the backend
load_timetable_data :-
    fetch_json('https://localhost:7081/api/Planning/get-timetable', TimetablesJSON),
    process_timetables(TimetablesJSON).

% Process timetable JSON and assert them as facts
process_timetables(TimetablesJSON) :-
    maplist(assert_timetable, TimetablesJSON),
    maplist(print_timetable, TimetablesJSON).

% Clear all timetable data from the knowledge base
clear_timetable_data :-
    retractall(timetable(_, _, _)),
    write('All timetable data cleared from the knowledge base.\n').

% Assert a timetable fact with Entrance and Exit times in minutes
assert_timetable(Timetable) :-
    % Converte o licenseNumber para átomo
    string_to_atom_safe(Timetable.get(licenseNumber, null), LicenseNumber),
    
    % A data não é convertida para átomo
    DateShift = Timetable.get(date, null),  % Deixe DateShift sem conversão para átomo
    
    % Obtemos os outros campos
    TimeShiftEntrance = Timetable.get(timeShiftEntrance, null),
    TimeShiftExit = Timetable.get(timeShiftExit, null),
    
    % Converte os horários de entrada e saída para minutos
    time_to_minutes(TimeShiftEntrance, EntranceTime),
    time_to_minutes(TimeShiftExit, ExitTime),

    % Assevera o fato de timetable na base de conhecimento
    assertz(timetable(LicenseNumber, DateShift, (EntranceTime, ExitTime))).

    
% Print timetable details to the console
print_timetable(Timetable) :-
    string_to_atom_safe(Timetable.get(licenseNumber, null), LicenseNumber),
    string_to_atom_safe(Timetable.get(date, null), DateShift),
    TimeShiftEntrance = Timetable.get(timeShiftEntrance, null),
    TimeShiftExit = Timetable.get(timeShiftExit, null),
    
    % Convert the times to minutes
    time_to_minutes(TimeShiftEntrance, EntranceTime),
    time_to_minutes(TimeShiftExit, ExitTime),

    % Print the timetable details
    format('Timetable for License Number: ~w\n', [LicenseNumber]),
    format('  Date: ~w\n', [DateShift]),
    format('  Entrance Time: ~w\n', [EntranceTime]),
    format('  Exit Time: ~w\n', [ExitTime]), nl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load surgeryId data from the backend and assert them as facts
load_surgeryId_data :-
    fetch_json('https://localhost:7081/api/Planning/get-surgeriesId', SurgeriesID),
    process_surgery_ids(SurgeriesID).

% Process surgeryId JSON and assert them as facts
process_surgery_ids(SurgeriesID) :-
    maplist(assert_surgery_id, SurgeriesID),
    maplist(print_surgery_ids, SurgeriesID). 

% Assert a surgery_id fact
assert_surgery_id(SurgeryIDData) :-
    string_to_atom_safe(SurgeryIDData.get(opRequestCode, null), OpRequestCode),
    string_to_atom_safe(SurgeryIDData.get(opTypeCode, null), OpTypeCode),
    assertz(surgery_id(OpRequestCode, OpTypeCode)).

% Clear all surgery_id data from the knowledge base
clear_surgery_id_data :-
    retractall(surgery_id(_, _)),
    write('All surgery_id data cleared from the knowledge base.\n').

% Predicado para imprimir todos os fatos surgery_id
print_surgery_ids(SurgeryIDData) :-
    string_to_atom_safe(SurgeryIDData.get(opRequestCode, null), OpRequestCode),
    string_to_atom_safe(SurgeryIDData.get(opTypeCode, null), OpTypeCode),
    format('Operation Request Code: ~w\n', [OpRequestCode]),
    format('Operation Type Code: ~w\n', [OpTypeCode]), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Clear all agenda_staff data
clear_agenda_staff_data :- 
    retractall(agenda_staff(_, _, _)),
    write('All agenda staff data cleared from the knowledge base.\n').

% Load agenda staff data from the backend
load_agenda_staff_data :- 
    fetch_json('https://localhost:7081/api/Planning/get-agenda-staff', AgendaStaffJSON),
    process_agenda_staff(AgendaStaffJSON).

% Process agenda staff JSON and assert them as facts
process_agenda_staff(AgendaStaffJSON) :- 
    maplist(assert_agenda_staff, AgendaStaffJSON),
    maplist(print_agenda_staff, AgendaStaffJSON).

% Assert a single agenda_staff fact
assert_agenda_staff(AgendaStaff) :- 
    % Extract LicenseNumber and Date from JSON and convert them to atoms
    get_dict(licenseNumber, AgendaStaff, LicenseNumberString),
    get_dict(date, AgendaStaff, DateString),
    get_dict(schedule, AgendaStaff, ScheduleStrings),
    
    % Convert LicenseNumber to atom (keep Date as string)
    string_to_atom(LicenseNumberString, LicenseNumber),
    Date = DateString,  % Keep the Date as a string
    
    % Process the schedule into a structured format
    maplist(parse_schedule_entry, ScheduleStrings, ParsedSchedule),
    
    % Assert the agenda staff fact
    assertz(agenda_staff(LicenseNumber, Date, ParsedSchedule)).

% Parse a schedule string "HH:MM:SS-HH:MM:SS-Code" into a structured term (Start, End, Code)
parse_schedule_entry(ScheduleString, (StartMinutes, EndMinutes, Code)) :- 
    % Split the schedule string into start, end, and code parts
    split_string(ScheduleString, "-", ",", [Start, End, Code]),
    
    % Convert start and end times into minutes since midnight
    time_to_minutes(Start, StartMinutes),
    time_to_minutes(End, EndMinutes).

% Print an agenda_staff entry
print_agenda_staff(AgendaStaff) :- 
    get_dict(licenseNumber, AgendaStaff, LicenseNumberString),
    get_dict(date, AgendaStaff, DateString),
    get_dict(schedule, AgendaStaff, ScheduleStrings),
    
    % Convert LicenseNumber to atom (keep Date as string)
    string_to_atom(LicenseNumberString, LicenseNumber),
    Date = DateString,  % Keep the Date as a string
    
    % Parse the schedule entries
    maplist(parse_schedule_entry, ScheduleStrings, ParsedSchedule),
    
    % Print the staff agenda
    format('Agenda Staff for License Number: ~w\n', [LicenseNumber]),
    format('  Date: ~w\n', [Date]),
    format('  Schedule: ~w\n', [ParsedSchedule]), nl.

% Print all agenda_staff facts
print_all_agenda_staff :- 
    write('Listing all agenda staff:\n'),
    forall(agenda_staff(LicenseNumber, Date, Schedule),
           format('License Number: ~w, Date: ~w, Schedule: ~w\n', [LicenseNumber, Date, Schedule])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load agenda operation room data from the backend
load_agenda_operation_room_data :- 
    fetch_json('https://localhost:7081/api/Planning/get-agenda-operation-room', AgendaOperationRoomJSON),
    process_agenda_operation_room(AgendaOperationRoomJSON).

% Process agenda operation room JSON and assert them as facts
process_agenda_operation_room(AgendaOperationRoomJSON) :-
    maplist(assert_agenda_operation_room, AgendaOperationRoomJSON),
    maplist(print_agenda_operation_room, AgendaOperationRoomJSON).

% Assert a single agenda_operation_room fact
assert_agenda_operation_room(AgendaRoom) :-
    % Extract roomNumber and date from JSON
    get_dict(roomNumber, AgendaRoom, RoomNumber),
    get_dict(date, AgendaRoom, Date),
    get_dict(schedule, AgendaRoom, ScheduleStrings),
    
    % Convert RoomNumber to atom (keep Date as string)
    string_to_atom(RoomNumber, RoomNumberAtom),
    Date = Date,  % Keep Date as a string (no conversion to atom)
    
    % Process each schedule string and convert to structured terms
    maplist(parse_schedule_entry, ScheduleStrings, ParsedSchedule),
    
    % Assert the agenda operation room fact
    assertz(agenda_operation_room(RoomNumberAtom, Date, ParsedSchedule)).

% Print an agenda_operation_room entry
print_agenda_operation_room(AgendaRoom) :-
    get_dict(roomNumber, AgendaRoom, RoomNumber),
    get_dict(date, AgendaRoom, Date),
    get_dict(schedule, AgendaRoom, ScheduleStrings),
    
    % Convert RoomNumber to atom (keep Date as string)
    string_to_atom(RoomNumber, RoomNumberAtom),
    Date = Date,  % Keep Date as a string (no conversion to atom)
    
    % Parse the schedule entries
    maplist(parse_schedule_entry, ScheduleStrings, ParsedSchedule),
    
    % Print the operation room agenda
    format('Agenda for Room: ~w, Date: ~w\n', [RoomNumberAtom, Date]),
    format('  Schedule: ~w\n', [ParsedSchedule]), nl.

% Print all agenda_operation_room facts
print_all_agenda_operation_rooms :- 
    write('Listing all agenda operation rooms:\n'),
    forall(agenda_operation_room(RoomNumber, Date, Schedule),
           format('Room Number: ~w, Date: ~w, Schedule: ~w\n', [RoomNumber, Date, Schedule])).

% Clear all agenda operation room data from the knowledge base
clear_agenda_operation_room_data :- 
    retractall(agenda_operation_room(_, _, _)),  % Remove all agenda_operation_room facts
    write('All agenda operation room data cleared from the knowledge base.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  % Load assignment surgery data from the backend
load_assignment_surgery_data :-
    fetch_json('https://localhost:7081/api/Planning/get-assignments', AssignmentsJSON),
    process_assignment_surgeries(AssignmentsJSON).


     % Process the assignment surgery JSON and assert them as facts
process_assignment_surgeries(AssignmentsJSON) :-
    maplist(assert_assignment_surgery_from_json, AssignmentsJSON),
    maplist(print_assignment_surgery_from_json, AssignmentsJSON).

    % Assert a assignment_surgery from JSON
assert_assignment_surgery_from_json(Assignment) :-
    get_dict(opRequestCode, Assignment, OpRequestCode),
    get_dict(licenseNumber, Assignment, LicenseNumber),
    string_to_atom(OpRequestCode, OpRequestCodeAtom),
    string_to_atom(LicenseNumber, LicenseNumberAtom),
    assert_assignment_surgery(OpRequestCodeAtom, LicenseNumberAtom).
    
    % Print assignment_surgery from JSON
print_assignment_surgery_from_json(Assignment) :-
    get_dict(opRequestCode, Assignment, OpRequestCode),
    get_dict(licenseNumber, Assignment, LicenseNumber),
    string_to_atom(OpRequestCode, OpRequestCodeAtom),
    string_to_atom(LicenseNumber, LicenseNumberAtom),
    print_assignment_surgery(OpRequestCodeAtom, LicenseNumberAtom).

  % Assert a assignment_surgery fact (associando operação à equipe).
assert_assignment_surgery(OpRequestCode, LicenseNumber) :-
    string_to_atom(OpRequestCode, OpRequestCodeAtom),
    string_to_atom(LicenseNumber, LicenseNumberAtom),
    assertz(assignment_surgery(OpRequestCodeAtom, LicenseNumberAtom)).

   % Print assignment_surgery details to the console
print_assignment_surgery(OpRequestCode, LicenseNumber) :-
    format('Assignment Surgery - OpRequestCode: ~w, LicenseNumber: ~w\n', [OpRequestCode, LicenseNumber]).

  % Clear all assignment surgery data from the knowledge base
clear_assignment_surgery_data :-
    retractall(assignment_surgery(_, _)),
    write('All assignment surgery data cleared from the knowledge base.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- use_module(library(http/thread_httpd)).
    :- use_module(library(http/http_dispatch)).
    :- use_module(library(http/http_json)).
    :- use_module(library(http/http_parameters)).
    :- use_module(library(lists)).
    :- use_module(library(http/http_cors)).
    
    :- set_prolog_flag(encoding, utf8).
    
    % Cors: Permitir requisições de qualquer origem
    :- set_setting(http:cors, [*]).
    
    % Rota para /obtain_better_solution
    :- http_handler(root(obtain_better_solution), handle_obtain_better_sol, []).
    
    % Rota para outras requisições, se necessário
    :- http_handler(root(sort_surgeries), handle_sort_surgeries, [method(get)]).
    
    % Servidor HTTP na porta 8080
    server(Port) :-
        http_server(http_dispatch, [port(Port)]).
    
    % Inicia o servidor automaticamente
    :- initialization(start_server).
    
    start_server :-
        server(8080),
        writeln('Servidor HTTP rodando na porta 8080').
    
    %--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    % Predicado para processar a requisição de /obtain_better_solution
    handle_obtain_better_sol(Request) :-
        % Ativa CORS para a requisição
        cors_enable,
    
        % Processa os parâmetros da requisição HTTP
        http_parameters(Request, [
            room(Room, [atom]),
            day(Day, [integer])
        ]),
    
        % Registra o tempo de início
        get_time(Ti),
    
        % Passa os parâmetros para a solução
        obtain_better_sol(Room, Day, AgOpRoomBetter, LAgDoctorsBetter, TFinOp),
    
        % Registra o tempo de término
        get_time(Tf),
    
        % Calcula o tempo de execução
        T is Tf - Ti,
    
        % Formata os resultados
        maplist(format_ag_op_room_better, AgOpRoomBetter, AgOpRoomBetterFormatted),
        maplist(format_doctors_agenda, LAgDoctorsBetter, LAgDoctorsBetterFormatted),
    
        % Responde com JSON
        reply_json_dict(_{
            status: "success",
            ag_op_room_better: AgOpRoomBetterFormatted,
            doctors_agenda_better: LAgDoctorsBetterFormatted,
            final_time: TFinOp,
            generation_time: T  % Tempo de geração da solução
        }, [encoding(utf8)]).
    
    % Função auxiliar para formatar AgOpRoomBetter
    format_ag_op_room_better((Start, End, SurgeryID), [Start, End, SurgeryID]).
    
    % Função auxiliar para formatar LAgDoctorsBetter
    format_doctors_agenda((DoctorID, Schedule), _{doctor: DoctorID, schedule: FormattedSchedule}) :-
        % Formatar cada agenda do médico
        maplist(format_schedule_entry, Schedule, FormattedSchedule).
    
    % Função auxiliar para formatar cada item da agenda
    format_schedule_entry((Start, End, SurgeryID), _{start_time: Start, end_time: End, surgery_id: SurgeryID}).











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Código para obter e tratar as agendas livres
free_agenda0([],[(0,1440)]).
free_agenda0([(0,Tfin,)|LT],LT1):-!,free_agenda1([(0,Tfin,)|LT],LT1).
free_agenda0([(Tin,Tfin,_)|LT],[(0,T1)|LT1]):- T1 is Tin-1,
    free_agenda1([(Tin,Tfin,_)|LT],LT1).

free_agenda1([(,Tfin,)],[(T1,1440)]):-Tfin\==1440,!,T1 is Tfin+1.
free_agenda1([(,,_)],[]).
free_agenda1([(,T,),(T1,Tfin2,_)|LT],LT1):-Tx is T+1,T1==Tx,!,
    free_agenda1([(T1,Tfin2,_)|LT],LT1).
free_agenda1([(,Tfin1,),(Tin2,Tfin2,_)|LT],[(T1,T2)|LT1]):-T1 is Tfin1+1,T2 is Tin2-1,
    free_agenda1([(Tin2,Tfin2,_)|LT],LT1).

adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,)|],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,)|],[(In,FinTime)]).
treatfin(_,[],[]).

% Intersecção das agendas para disponibilidade dos médicos e sala
intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

intersect_2_agendas([],_,[]).
intersect_2_agendas([D|LD],LA,LIT):-	intersect_availability(D,LA,LI,LA1),
					intersect_2_agendas(LD,LA1,LID),
					append(LI,LID,LIT).

intersect_availability((,),[],[],[]).

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

% Agendamento das cirurgias
schedule_all_surgeries(Room,Day):-
    retractall(agenda_staff1(,,_)),
    retractall(agenda_operation_room1(,,_)),
    retractall(availability(,,_)),
    findall(,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),),
    findall(OpCode,surgery_id(OpCode,_),LOpCode),
    availability_all_surgeries_with_all_staff(LOpCode,Room,Day),!.


% Verificação de disponibilidade e atualização das agendas
availability_all_surgeries_with_all_staff([], _, _).
availability_all_surgeries_with_all_staff([OpCode | LOpCode], Room, Day) :-
    (   
        % Tentar agendar a operação atual
        surgery_id(OpCode, OpType),
        surgery(OpType, TAnesthesia, TSurgery, TCleaning),
        TTotal is TAnesthesia + TSurgery + TCleaning,
        availability_operation(OpCode, Room, Day, LPossibilities, LDoctors),
        schedule_first_interval(TTotal, LPossibilities, (TinS, TfinS)),
        retract(agenda_operation_room1(Room, Day, Agenda)),
        insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),
        assertz(agenda_operation_room1(Room, Day, Agenda1)),

        % Agendar anestesista
        obtain_staff_speciality(OpCode, anesthesia, LAnesthesia),
        TinAnesthesia is TinS,
        TfinAnesthesia is TinS + TAnesthesia + TSurgery,
        insert_agenda_doctors((TinAnesthesia, TfinAnesthesia, OpCode), Day, LAnesthesia),

        % Agendar cirurgião
        obtain_staff_speciality(OpCode, orthopaedist, LSurgery),
        TinSurgery is TinS + TAnesthesia + 1,
        TfinSurgery is TinSurgery + TSurgery,
        insert_agenda_doctors((TinSurgery, TfinSurgery, OpCode), Day, LSurgery),

        % Agendar limpeza
        obtain_staff_speciality(OpCode, cleaning, LCleaning),
        TinCleaning is TfinSurgery + 1,
        TfinCleaning is TfinSurgery + TCleaning,
        insert_agenda_doctors((TinCleaning, TfinCleaning, OpCode), Day, LCleaning)
    ->  true
    ;   % Caso não consiga agendar a operação, exibir aviso
    ),
    % Continuar com as próximas operações
    availability_all_surgeries_with_all_staff(LOpCode, Room, Day).

        
        
        obtain_staff_speciality(OpCode, Specialty, LStaff) :-
            surgery_id(OpCode, SurgeryType),  % Obter tipo de cirurgia associado ao OpCode
            findall(StaffId, (
                staff(StaffId, _, Specialty, Specialties), % Buscar por membros da equipe com a especialidade desejada
                member(SurgeryType, Specialties)          % Verificar se a especialidade cobre o tipo da cirurgia
            ), LStaff).

% MODIFIED
availability_operation(OpCode, Room, Day, LPossibilities, LDoctors) :-
    surgery_id(OpCode, OpType),
    surgery(OpType, TAnesthesia, TSurgery, TCleaning),
    TotalTime is TAnesthesia + TSurgery + TCleaning,  % Tempo total (anestesia, cirurgia, limpeza)
    findall(Doctor, assignment_surgery(OpCode, Doctor), LDoctors),
    intersect_all_agendas(LDoctors, Day, LA),
    agenda_operation_room1(Room, Day, LAgenda),
    free_agenda0(LAgenda, LFAgRoom),
    intersect_2_agendas(LA, LFAgRoom, LIntAgDoctorsRoom),
    remove_unf_intervals(TotalTime, LIntAgDoctorsRoom, LPossibilities).  % Verifica disponibilidade para o tempo total

% MODIFIED
remove_unf_intervals(_, [], []).
remove_unf_intervals(TotalTime, [(Tin, Tfin) | LA], [(Tin, Tfin) | LA1]) :-
    DT is Tfin - Tin + 1,
    TotalTime =< DT,  % Verifica se o intervalo comporta o tempo total (anestesia + cirurgia + limpeza)
    !,
    remove_unf_intervals(TotalTime, LA, LA1).
remove_unf_intervals(TotalTime, [_ | LA], LA1) :-
    remove_unf_intervals(TotalTime, LA, LA1).


schedule_first_interval(TotalTime, [(Tin, _) | _], (Tin, TfinS)) :-
    TfinS is Tin + TotalTime - 1.  

insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).

insert_agenda_doctors(,,[]).
insert_agenda_doctors((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors).

%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
obtain_better_sol(Room, Day, AgOpRoomBetter, LAgDoctorsBetter, TFinOp) :-
    get_time(Ti),
    (obtain_better_sol1(Room, Day) ; true),
    retract(better_sol(Day, Room, AgOpRoomBetter, LAgDoctorsBetter, TFinOp)),
    write('Final Result: AgOpRoomBetter='), write(AgOpRoomBetter), nl,
    write('LAgDoctorsBetter='), write(LAgDoctorsBetter), nl,
    write('TFinOp='), write(TFinOp), nl,
    get_time(Tf),
    T is Tf - Ti,
    write('Tempo de geracao da solucao: '), write(T), nl.

% CONTINUA A OBTER DUPLICADOS MAS ENVOLVE TODA A STAFF
obtain_better_sol1(Room, Day) :-
    asserta(better_sol(Day, Room, _, _, 1441)),
    findall(OpCode, surgery_id(OpCode, _), LOC),
    write('Lista de OpCodes: '), write(LOC), nl, % Adicionando write para ver a lista de cirurgias
    permutation(LOC, LOpCode),
    write('Permutação de OpCodes: '), write(LOpCode), nl, % Escrevendo a permutação
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),
    agenda_operation_room(Room, Day, Agenda),
    assert(agenda_operation_room1(Room, Day, Agenda)),
    findall(_, (agenda_staff1(D, Day, L), free_agenda0(L, LFA), adapt_timetable(D, Day, LFA, LFA2), assertz(availability(D, Day, LFA2))), _),
    availability_all_surgeries_with_all_staff(LOpCode, Room, Day),
    agenda_operation_room1(Room, Day, AgendaR),
    write('Agenda Operação Sala Atual: '), write(AgendaR), nl, % Mostra a agenda após as cirurgias
    update_better_sol(Day, Room, AgendaR, LOpCode),
    fail.

update_better_sol(Day, Room, Agenda, LOpCode) :-
    better_sol(Day, Room, _, _, FinTime),
    reverse(Agenda, AgendaR),
    evaluate_final_time(AgendaR, LOpCode, FinTime1),
    write('Analisando para LOpCode='), write(LOpCode), nl,
    write('Agora: FinTime1='), write(FinTime1), write(' Agenda='), write(Agenda), nl,
    FinTime1 < FinTime,
    write('Melhor solução atualizada'), nl,
    retract(better_sol(_, _, _, _, _)),
    % Obter agendas dos médicos
    findall(Doctor, assignment_surgery(_, Doctor), LDoctors1),
    remove_equals(LDoctors1, LDoctors),
    

    % Obter agendas de todo o staff
    generate_staff_agendas(Day, LStaffAgendas),

    asserta(better_sol(Day, Room, Agenda, [LDAgendas, LStaffAgendas], FinTime1)).


evaluate_final_time([], _, 1441).
evaluate_final_time([(_, Tfin, _) | _], LOpCode, Tfin) :-
    member(_, LOpCode), !.  % Aqui, a variável OpCode não está sendo usada diretamente.
evaluate_final_time([(_, Tfin, _) | AgR], LOpCode, TfinFinal) :-
    evaluate_final_time(AgR, LOpCode, TfinTemp),
    TfinFinal is max(Tfin, TfinTemp).

% Função que gera a lista de médicos associados à agenda
list_doctors_agenda(_, [], []).

list_doctors_agenda(Day, [D | LD], [(D, UniqueAg) | LAgD]) :-
    agenda_staff1(D, Day, AgD),
    remove_equals(AgD, UniqueAg),  % Remover duplicatas na agenda
    list_doctors_agenda(Day, LD, LAgD).

    
% Função que remove duplicatas de uma lista
remove_equals([], []).
remove_equals([X | L], L1) :-
    member(X, L), !,
    remove_equals(L, L1).
remove_equals([X | L], [X | L1]) :-
    remove_equals(L, L1).

%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
list_staff_agenda(_, [], []).
list_staff_agenda(Day, [Staff | LStaff], [(Staff, UniqueAgStaff) | LAgStaff]) :-
    agenda_staff1(Staff, Day, AgStaff),
    remove_equals(AgStaff, UniqueAgStaff),  % Remover duplicatas na agenda
    list_staff_agenda(Day, LStaff, LAgStaff).

generate_staff_agendas(Day, StaffAgendas) :-
    % Obter IDs de todos os membros do staff
    findall(StaffId, (
        agenda_staff1(StaffId, Day, _)
    ), AllStaffIds),
    remove_equals(AllStaffIds, UniqueStaffIds),  % Remover duplicatas
    list_staff_agenda(Day, UniqueStaffIds, StaffAgendas).