using Microsoft.AspNetCore.Mvc;
using Sempi5.Domain.AgendaAggregate;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Services;

namespace Sempi5.Controllers;

[ApiController]
[Route("[controller]")]
public class AlgavController : ControllerBase
{
    private readonly StaffService _staffService;
    private readonly SurgeryRoomService _surgeryRoomService;
    private readonly OperationRequestService _operationRequestService;
    private readonly OperationTypeService _operationTypeService;
    private readonly AgendaService _agendaService;

    public AlgavController(
        StaffService staffService,
        SurgeryRoomService surgeryRoomService,
        OperationRequestService operationRequestService,
        OperationTypeService operationTypeService,AgendaService agendaService)
    {
        _staffService = staffService;
        _surgeryRoomService = surgeryRoomService;
        _operationRequestService = operationRequestService;
        _operationTypeService = operationTypeService;
        _agendaService = agendaService;
    }

    [HttpGet("staff")]
    public async Task<IActionResult> GetStaff()
    {
        var staffList = await _staffService.getStaff();
        var tableData = staffList.Select(staff => new
        {
            Id = staff.Id,
            LicenseNumber = staff.LicenseNumber?.ToString(),
            Name = staff.Person,
            Specialization = staff.Specialization,
            Status = staff.Status.ToString(),
            AvailabilitySlots = staff.AvailabilitySlots,
            StaffAgenda=staff.StaffAgendas
        }).ToList();

        return Ok(tableData);
    }

    [HttpGet("surgery-room")]
    public async Task<IActionResult> GetSurgeryRoom()
    {
        var rooms = await _surgeryRoomService.getSurgeryRoom();
        var tableData = rooms.Select(room => new
        {
            Id = room.Id?.Value,
            Type = room.Type.ToString(),
            Capacity = room.Capacity,
            Status = room.Status.ToString(),
            Equipment = room.Equipment,
            MaintenanceSlots = room.MaintenanceSlots,
            RoomAgenda=room.RoomAgendas
            
        }).ToList();

        return Ok(tableData);
    }

    [HttpGet("operation-request")]
    public async Task<IActionResult> GetOperationRequest()
    {
        var requests = await _operationRequestService.getallOperationRequest();
       
        var tableData = requests.Select(request => new
        {
            Id = request.Id?.Value,
            DoctorName = request.Doctor.Person?.FullName.ToString(),
            PatientName = request.Patient.Person?.FullName.ToString(),
            OperationType = request.OperationType?.Name.ToString(),
            Deadline = request.DeadLineDate.ToString("yyyy-MM-dd"),
            Priority = request.PriorityEnum.ToString(),
          //  Specialization=request.Doctor.Specialization.specializationName.ToString()
        }).ToList();

        return Ok(tableData);
    }

    [HttpGet("operation-type")]
    public async Task<IActionResult> GetOperationType()
    {
        var operationTypes = await _operationTypeService.getOperationType();
        var tableData = operationTypes.Select(operationType => new
        {
            Id = operationType.Id?.Value,
            Name = operationType.Name.ToString(),
            SetupDuration = operationType.SetupDuration.ToString(),
            SurgeryDuration = operationType.SurgeryDuration.ToString(),
            CleaningDuration = operationType.CleaningDuration.ToString(),
            StillPerformed = operationType.stillPerformed,
            RequiredStaff=operationType.RequiredStaff,
        }).ToList();

        return Ok(tableData);
    }
    [HttpGet("RequiredStaff")]
    public async Task<IActionResult> GetRequiredStaff()
    {
        var requiredStaff = await _operationTypeService.getRequiredStaff();
        var tableData = requiredStaff.Select(requiredStaff => new
        {
            Id = requiredStaff.Id?.Value,
            NumberOfStaff = requiredStaff.NumberOfStaff,
            Specialization = requiredStaff.Specialization.specializationName.ToString()
        }).ToList();

        return Ok(tableData);
    }
    // [HttpGet("room-agenda")]
    // public async Task<IActionResult> GetRoomAgenda()
    // {
    //     var roomAgenda = await _agendaService.getAllRoomAgenda();
    //     var tableData = roomAgenda.Select(roomAgenda => new
    //     {
    //         Date = roomAgenda.Date,
    //         Intervals = roomAgenda.TimeIntervals,
    //     }).ToList();
    //
    //     return Ok(tableData);
    // }
    // [HttpGet("staff-agenda")]
    // public async Task<IActionResult> GetStaffAgenda()
    // {
    //     var staffAgenda = await _agendaService.getAllStaffAgenda();
    //     var tableData = staffAgenda.Select(staffAgenda => new
    //     {
    //         Date = staffAgenda.Date,
    //         Intervals = staffAgenda.TimeIntervals
    //     }).ToList();
    //
    //     return Ok(tableData);
    // }
    // [HttpPost("appointment")]
    // public async Task<IActionResult>createappointment(AppointmentHistoryDTO appointment)
    // {
    //     await _agendaService.createAppointment(appointment);
    //     return Ok(new { message = "Appointment was created" });
    // }
    
    [HttpPatch("updateStaffAgenda")]
    public async Task<IActionResult> updateStaffAgenda(AgendaDto agenda)
    {        

       await _agendaService.updateAgendaStaff(agenda);
       return Ok(new { message = "Staff Agenda was updated" });
    }
    
    [HttpPatch("updateRoomAgenda")]
    public async Task<IActionResult> updateRoomAgenda(AgendaDto agenda)
    {      
      await _agendaService.updateAgendaRoom(agenda);
      return Ok(new { message = "Room Agenda was updated" });
    }

}
