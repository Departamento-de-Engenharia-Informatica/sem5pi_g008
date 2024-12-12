:- dynamic surgery/4.
:- dynamic staff/4.
:- dynamic timetable/3.
:- dynamic surgery_id/2.
:- dynamic agenda_staff/3.
:- dynamic agenda_operation_room/3.
:- dynamic surgery_staff_requirements/3.

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).


:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

% fetch_json(URL, JSON)
% Fetch JSON data from a given URL.
fetch_json(URL, JSON) :-
    http_open(URL, Stream, []),       % Open the URL and get the response stream
    json_read_dict(Stream, JSON),     % Parse the stream into a JSON dictionary
    close(Stream).                    % Close the stream after use



% Load all data


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process and load operation request data
process_operation_requests([]):-!.
process_operation_requests([Request | Rest]) :-
    % Extract fields safely
    string_to_atom_safe(Request.get(id), ID),
    Deadline = Request.get(deadline),
    (   Request.get(doctorName) == null 
    ->  DoctorName = null
    ;   string_to_atom_safe(Request.get(doctorName), DoctorName)
    ),
    (   Request.get(operationType) == null 
    ->  OperationType = null
    ;   string_to_atom_safe(Request.get(operationType), OperationType)
    ),
    (   Request.get(patientName) == null 
    ->  PatientName = null
    ;   string_to_atom_safe(Request.get(patientName), PatientName)
    ),
    string_to_atom_safe(Request.get(priority), Priority),
    
    % Assert operation_request fact
    assertz(operation_request(ID, Deadline, DoctorName, OperationType, PatientName, Priority)),
    assertz(surgery_id(ID,OperationType)),
    
    % Print confirmation for debugging
    format('Operation Request ~w: Deadline: ~w, Doctor: ~w, Operation Type: ~w, Patient: ~w, Priority: ~w\n',
           [ID, Deadline, DoctorName, OperationType, PatientName, Priority]),
           !,
    process_operation_requests(Rest).

% Main loader predicate
load_operation_requests :-
    fetch_json('http://localhost:5001/algav/operation-request', OperationRequestsJSON),
    process_operation_requests(OperationRequestsJSON).

% Clear all operation request data
clear_operation_requests :-
    retractall(operation_request(_, _, _, _, _, _)),
    write('Operation request data cleared.\n').

% Utility: Safe conversion of string to atom
string_to_atom_safe(String, Atom) :-
    (var(String) -> Atom = 'Unknown'; string_to_atom(String, Atom)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process and load operation-type data
process_operation_types([]):-!.
process_operation_types([Operation | Rest]) :-
    % Extract fields
    string_to_atom_safe(Operation.get(id), OperationID),
    time_to_minutes(Operation.get(cleaningDuration), CleaningDuration),
    time_to_minutes(Operation.get(surgeryDuration), SurgeryDuration),
    time_to_minutes(Operation.get(setupDuration), SetupDuration),
    StillPerformed = Operation.get(stillPerformed),
    % Assert the operation-type fact
    assertz(operation_type(OperationID, SetupDuration, SurgeryDuration, CleaningDuration, StillPerformed)),
    assertz(surgery(OperationID, SetupDuration, SurgeryDuration, CleaningDuration)),
    % Print confirmation
    format('Operation ~w: Setup: ~w min, Surgery: ~w min, Cleaning: ~w min, Still Performed: ~w\n',
           [OperationID, SetupDuration, SurgeryDuration, CleaningDuration, StillPerformed]),
           !,
    process_operation_types(Rest).

% Main loader predicate
load_operation_types :-
    fetch_json('http://localhost:5001/algav/operation-type', OperationTypesJSON),
    process_operation_types(OperationTypesJSON).

% Clear all operation-type data
clear_operation_types :-
    retractall(operation_type(_, _, _, _, _)),
    write('Operation type data cleared.\n').

% Utility: Time conversion
time_to_minutes(TimeString, Minutes) :-
    split_string(TimeString, ":", "", [HStr, MStr, _]),
    number_string(Hours, HStr),
    number_string(MinutesPart, MStr),
    TotalMinutes is Hours * 60 + MinutesPart,
    Minutes is TotalMinutes,!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definir predicados dinâmicos
:- dynamic surgery_room/5.
:- dynamic room_agenda/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% process_rooms/1
% Processa a lista completa de salas de cirurgia.
process_rooms([]) :- !.  % Caso base
process_rooms([Room | Rest]) :-
    % Extrair campos principais
    string_to_atom_safe(Room.id, RoomID),
    string_to_atom_safe(Room.type, RoomType),
    string_to_atom_safe(Room.status, RoomStatus),
    Room.capacity.capacity = Capacity,
    Room.equipment = Equipment,

    % Armazenar o fato da sala
    assertz(surgery_room(RoomID, RoomType, Capacity, Equipment, RoomStatus)),

    % Processar agenda associada à sala
    RoomAgenda = Room.roomAgenda,
    process_room_agenda(RoomID, RoomAgenda),

    % Exibir confirmação
    format('Room ~w: Type ~w, Capacity ~w, Status ~w, Equipment ~w\n',
           [RoomID, RoomType, Capacity, RoomStatus, Equipment]),
    !,
    process_rooms(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% process_room_agenda/2
% Processa a agenda associada a uma sala específica.
process_room_agenda(_, []) :- !.  % Caso base
process_room_agenda(RoomID, [AgendaEntry | Rest]) :-
    % Extrair data e intervalos da agenda
    string_to_atom_safe(AgendaEntry.date, DateISO),
    date_iso_to_ymd(DateISO, FormattedDate),
    Intervals = AgendaEntry.timeIntervals,

    % Armazenar os intervalos da agenda
    process_room_intervals(RoomID, FormattedDate, Intervals),
    !,
    process_room_agenda(RoomID, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% process_room_intervals/3
% Processa os intervalos de tempo associados a uma sala e data.
process_room_intervals(_, _, []) :- !.  % Caso base
process_room_intervals(RoomID, Date, [Interval | Rest]) :-
    assertz(room_agenda(RoomID, Date, Interval)),
    format('Room Agenda: RoomID ~w, Date ~w, Interval ~w\n', [RoomID, Date, Interval]),
    !,
    process_room_intervals(RoomID, Date, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Converter datas ISO (ex: "2024-12-15T00:00:00") para formato YYYYMMDD.
date_iso_to_ymd(DateISO, FormattedDate) :-
    split_string(DateISO, "T", "", [DatePart | _]),
    split_string(DatePart, "-", "", [Year, Month, Day]),
    atomic_list_concat([Year, Month, Day], '', FormattedDate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Função auxiliar para converter strings em átomos com segurança.
string_to_atom_safe(String, Atom) :-
    ( var(String) -> Atom = 'Unknown' ; string_to_atom(String, Atom) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado principal para carregar os dados JSON e processar
load_surgery_rooms :-
    fetch_json('http://localhost:5001/algav/surgery-room', RoomsJSON),
    process_rooms(RoomsJSON).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process and load required staff data
process_required_staff([]):-!.
process_required_staff([Staff | Rest]) :-
    % Extract fields
    string_to_atom_safe(Staff.get(id), ID),
    NumberOfStaff = Staff.numberOfStaff.get(value),
    (   Staff.specialization == null 
    ->  Specialization = null
    ;   string_to_atom_safe(Staff.get(specialization), Specialization)
    ),
    % Assert the required_staff fact
    assertz(surgery_staff_requirements(ID, NumberOfStaff, Specialization)),
    assertz(required_staff(ID, NumberOfStaff, Specialization)),
    % Print confirmation
    format('Required Staff ~w: Number of Staff: ~w, Specialization: ~w\n',
           [ID, NumberOfStaff, Specialization]),!,
    process_required_staff(Rest).

% Main loader predicate
load_required_staff :-
    fetch_json('http://localhost:5001/algav/RequiredStaff', RequiredStaffJSON),
    process_required_staff(RequiredStaffJSON).

% Clear all required staff data
clear_required_staff :-
    retractall(required_staff(_, _, _)),
    write('Required staff data cleared.\n').

% Utility: Safe conversion of string to atom
% Declarar os fatos dinâmicos
:- dynamic staff/4.
:- dynamic staff_agenda/3.

% load_staff/0
% Carrega e processa os dados de membros da equipe.
load_staff :-
    % Obter os dados da equipe (substitua pela URL ou caminho correto)
    fetch_json('http://localhost:5001/algav/staff', StaffJSON),
    
    % Processar os dados da equipe
    process_staff(StaffJSON).

% process_staff/1
% Processa a lista completa de membros da equipe.
process_staff([]) :- !.  % Caso base
process_staff([Staff | Rest]) :-
    % Extrair campos principais
    string_to_atom_safe(Staff.id.value, StaffID),
    string_to_atom_safe(Staff.licenseNumber, LicenseNumber),
    string_to_atom_safe(Staff.specialization.specializationName.name, Specialization),
    string_to_atom_safe(Staff.status, Status),

    % Armazenar os fatos do staff
    assertz(staff(StaffID, LicenseNumber, Specialization, Status)),

    % Processar agenda associada ao staff
    StaffAgenda = Staff.staffAgenda,
    process_staff_agenda_entries(StaffID, StaffAgenda),

    % Exibir confirmação para depuração
    format('Staff ~w: License ~w, Specialization ~w, Status ~w\n',
           [StaffID, LicenseNumber, Specialization, Status]),
    !,
    process_staff(Rest).

% process_staff_agenda_entries/2
% Processa as entradas de agenda para um membro específico do staff.
process_staff_agenda_entries(_, []) :- !.  % Caso base
process_staff_agenda_entries(StaffID, [AgendaEntry | Rest]) :-
    % Extrair data e intervalos da agenda
    string_to_atom_safe(AgendaEntry.date, DateISO),
    date_iso_to_ymd(DateISO, FormattedDate),  % Converter ISO para formato YYYYMMDD
    Intervals = AgendaEntry.timeIntervals,

    % Armazenar cada intervalo como fato associado ao staff
    process_staff_intervals(StaffID, FormattedDate, Intervals),
    !,
    process_staff_agenda_entries(StaffID, Rest).

% process_staff_intervals/3
% Processa os intervalos de tempo associados a um staff e uma data.
process_staff_intervals(_, _, []) :- !.  % Caso base
process_staff_intervals(StaffID, Date, [Interval | Rest]) :-
    assertz(staff_agenda(StaffID, Date, Interval)),
    format('Staff Agenda: StaffID ~w, Date ~w, Interval ~w\n', [StaffID, Date, Interval]),
    !,
    process_staff_intervals(StaffID, Date, Rest).

% Converter datas ISO (ex: "2024-12-15T00:00:00") para formato YYYYMMDD.
date_iso_to_ymd(DateISO, FormattedDate) :-
    split_string(DateISO, "T", "", [DatePart | _]),
    split_string(DatePart, "-", "", [Year, Month, Day]),
    atomic_list_concat([Year, Month, Day], '', FormattedDate).

% Função auxiliar para converter strings em átomos com segurança.
string_to_atom_safe(String, Atom) :-
    ( var(String) -> Atom = 'Unknown' ; string_to_atom(String, Atom) ).



% Reload all operation request data
reload_operation_requests :-
    clear_operation_requests,
    load_operation_requests.

% Reload all operation-type data
reload_operation_types :-
    clear_operation_types,
    load_operation_types.

% Reload all staff data
reload_staff_data :-
    clear_staff_data,
    load_staff_data.

% Reload all surgery rooms data
reload_surgery_rooms_data :-
    clear_surgery_rooms,
    load_surgery_rooms_data.

% Clear surgery rooms data (helper for reload)
clear_surgery_rooms :-
    retractall(surgery_room(_, _, _, _)),
    write('Surgery rooms data cleared.\n').

% Reload all required staff data
reload_required_staff_data :-
    clear_required_staff,
    load_required_staff.

% Reload all data
reload_all_data :-
    reload_operation_requests,
    reload_operation_types,
    reload_staff_data,
    reload_surgery_rooms_data,
    reload_required_staff_data,
    write('All data reloaded successfully.\n').
