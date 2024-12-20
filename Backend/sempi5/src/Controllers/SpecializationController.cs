using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using Sempi5.Domain.SpecializationAggregate;
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
    [Authorize(Roles = "Admin,Doctor")]
    public async Task<IActionResult> SpecializationByName(string specializationName)
    {
        try
        {
            var specializationDTO = await _specializationService.SpecializationByName(specializationName);

            return Ok(specializationDTO);
        }
        catch (SpecializationNotFoundException e)
        {
            return StatusCode(e.StatusCode, e.Message);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpGet]
    [Authorize(Roles = "Admin,Doctor")]
    public async Task<IActionResult> ListAllSpecializations()
    {
        try
        {
            var specializations = await _specializationService.ListAllSpecializations();
            return Ok(specializations);
        }
        catch (NoSpecializationsFoundException e)
        {
            return StatusCode(e.StatusCode, e.Message);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpDelete("{specializationName}")]
    [Authorize(Roles = "Admin")]
    public async Task<IActionResult> DeleteSpecialization(string specializationName)
    {
        try
        {
            await _specializationService.DeleteSpecialization(specializationName);

            return Ok(new { message = "Staff deactivated successfully." });
        }
        catch (SpecializationNotFoundException e)
        {
            return StatusCode(e.StatusCode, e.Message);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpPost("")]
    [Authorize(Roles = "Admin")]
    public async Task<IActionResult> CreateSpecialization(SpecializationDTO specializationDTO)
    {
        try
        {
            var speciliazationDTO = await _specializationService.CreateSpecialization(specializationDTO);

            return Ok(speciliazationDTO);
        }
        catch (SpecializationInUseException e)
        {
            return StatusCode(e.StatusCode, e.Message);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpPatch("name/{specializationId}")]
    [Authorize(Roles = "Admin")]
    public async Task<IActionResult> UpdateSpecializationName(int specializationId,[FromBody] string specializationName)
    {
        try
        {
            await _specializationService.UpdateSpecializationName(specializationId, specializationName);
            return NoContent();
        }
        catch (SpecializationNotFoundException e)
        {
            return NotFound(e.Message);
        }
        catch (Exception e)
        {
            return StatusCode(500, e.Message);
        }
    }
    
    [HttpPatch("description/{specializationId}")]
    [Authorize(Roles = "Admin")]
    public async Task<IActionResult> UpdateSpecializationDescription(int specializationId,[FromBody] string specializationDescription)
    {
        try
        {
            await _specializationService.UpdateSpecializationDescription(specializationId, specializationDescription);
            return NoContent();
        }
        catch (SpecializationNotFoundException e)
        {
            return StatusCode(601, e.Message);
        }
        catch (Exception e)
        {
            return StatusCode(500, e.Message);
        }
    }
    
}