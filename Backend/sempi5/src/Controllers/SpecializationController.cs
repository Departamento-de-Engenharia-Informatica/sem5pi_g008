using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using Sempi5.Domain.SpecializationAggregate.SpecializationExceptions;
using Sempi5.Services;

namespace Sempi5.Controllers;

[Route("[controller]")]
[ApiController]
public class SpecializationController : ControllerBase
{
    private readonly SpecializationService _specializationService;

    public SpecializationController(SpecializationService specializationService)
    {
        _specializationService = specializationService;
    }

    [HttpGet("{specializationName}")]
    public async Task<IActionResult> SpecializationByName(string specializationName)
    {
        try
        {
            var specialization = await _specializationService.SpecializationByName(specializationName);
            return Ok(specialization);
        }
        catch (SpecializationNotFoundException e)
        {
            return BadRequest(e.Message + " " + e.StatusCode);
        }
    }

    [HttpGet]
    // [Authorize(Roles = "Admin")]
    public async Task<IActionResult> ListAllSpecializations()
    {
        try
        {
            var specializations = await _specializationService.ListAllSpecializations();
            return Ok(specializations);
        }
        catch (NoSpecializationsFoundException e)
        {
            return BadRequest(e.Message + " --------------- " + e.StatusCode);
        }
    }
}