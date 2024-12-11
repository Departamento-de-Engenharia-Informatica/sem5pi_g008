using Microsoft.AspNetCore.Mvc;
using Sempi5.Domain.RequiredStaffAggregate;
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

    public AlgavController(
        StaffService staffService,
        SurgeryRoomService surgeryRoomService,
        OperationRequestService operationRequestService,
        OperationTypeService operationTypeService)
    {
        _staffService = staffService;
        _surgeryRoomService = surgeryRoomService;
        _operationRequestService = operationRequestService;
        _operationTypeService = operationTypeService;
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
            AvailabilitySlots = staff.AvailabilitySlots
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
            MaintenanceSlots = room.MaintenanceSlots
        }).ToList();

        return Ok(tableData);
    }

    [HttpGet("operation-request")]
    public async Task<IActionResult> GetOperationRequest()
    {
        var requests = await _operationRequestService.getOperationRequest();
        for (int i = 0; i < requests.Count; i++)
        {
            Console.Write("aquiiiiiiiiii"+requests[i].Doctor.Person?.FullName._name);
        }
        var tableData = requests.Select(request => new
        {
            Id = request.Id?.Value,
            DoctorName = request.Doctor?.Id.AsString(),
            PatientName = request.Patient,
            OperationType = request.OperationType?.Name,
            Deadline = request.DeadLineDate.ToString("yyyy-MM-dd"),
            Priority = request.PriorityEnum.ToString(),
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
            Name = operationType.Name,
            SetupDuration = operationType.SetupDuration.ToString(),
            SurgeryDuration = operationType.SurgeryDuration.ToString(),
            CleaningDuration = operationType.CleaningDuration.ToString(),
            StillPerformed = operationType.stillPerformed
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
            Specialization = requiredStaff.Specialization
        }).ToList();

        return Ok(tableData);
    }
}
