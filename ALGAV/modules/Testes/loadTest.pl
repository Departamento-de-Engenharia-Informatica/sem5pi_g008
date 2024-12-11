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
% Process and load staff data
process_staff([]):-!.
process_staff([Staff | Rest]) :-
    % Extract fields
    string_to_atom_safe(Staff.get(licenseNumber), LicenseNumber),
    Status = Staff.get(status),
    Specialization = Staff.specialization.specializationName.name,
    StaffID = Staff.id.value,
    % Assert the staff fact
        assertz(staff(StaffID, LicenseNumber, Specialization, Status)),

    assertz(agenda_staff(StaffID, 20240113, [])),
    % Print confirmation
    format('Staff ~w: Specialization: ~w, Status: ~w, ID: ~w\n',
           [LicenseNumber, Specialization, Status, StaffID]),!,
    process_staff(Rest).

% Main loader predicate
load_staff_data :-
    fetch_json('http://localhost:5001/algav/staff', StaffJSON),
    process_staff(StaffJSON).

% Clear all staff data
clear_staff_data :-
    retractall(staff(_, _, _, _)),
    write('Staff data cleared.\n').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_rooms([]):-!.
process_rooms([Room | Rest]) :-
    % Extract necessary fields
    string_to_atom_safe(Room.id, RoomID),
    string_to_atom_safe(Room.type, RoomType),
    string_to_atom_safe(Room.status, RoomStatus),
    RoomCapacity = Room.capacity.capacity, % Nested field extraction
    % Assert the room fact
    assertz(surgery_room(RoomID, RoomType, RoomStatus, RoomCapacity)),
    assertz(agenda_operation_room(RoomID, 20240113, [])),
    % Print confirmation
    format('Room ~w (~w): Status ~w, Capacity ~w\n', [RoomID, RoomType, RoomStatus, RoomCapacity]),!,
    process_rooms(Rest).

% Example for loading all rooms
load_surgery_rooms_data :-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
