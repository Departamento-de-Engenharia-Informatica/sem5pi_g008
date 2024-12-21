using Microsoft.AspNetCore.Authorization;
using Sempi5.Domain.AppointmentAggregate.DTO;
using Sempi5.Services;
using Microsoft.AspNetCore.Http.HttpResults;
using Microsoft.AspNetCore.Mvc;


namespace Sempi5.Controllers;

[ApiController]
[Route("[controller]")]
public class AppointmentController: ControllerBase
{
    private readonly AppointmentService _appointmentService;

    public AppointmentController(AppointmentService appointmentService)
    {
        _appointmentService = appointmentService;
    }   
    
    [HttpPost]
    public async Task<IActionResult> createAppointment(AppointmentDto appointmentDto)
    {
      //TODO: Implementar validação de dados
      //TODO: Mostrar status da operations request
        try
        {
            await _appointmentService.createAppointment(appointmentDto);
        }
        catch (Exception e)
        {
            return BadRequest("Error: " + e.Message);
        }

        return Ok("Appointment Created: " + appointmentDto);
    }
    [HttpPatch]
    [Authorize(Roles = "Doctor")]

    public async Task<IActionResult> updateAppointment(AppointmentDto appointmentDto)
    {
        try
        {
            await _appointmentService.updateAppointment(appointmentDto);
        }
        catch (Exception e)
        {
            return BadRequest("Error: " + e.Message);
        }

        return Ok("Appointment Updated: " + appointmentDto);
    }
}