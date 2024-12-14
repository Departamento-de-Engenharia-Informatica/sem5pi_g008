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
        
        Console.WriteLine("\n \n \n");
        Console.WriteLine("ENTROU AQUI: - " + specializationName);
        Console.WriteLine("\n \n \n");

        try
        {
            var specialization = await _specializationService.SpecializationByName(specializationName);
            
            Console.WriteLine("\n \n \n");
            Console.WriteLine(specialization.specializationName);
            Console.WriteLine("\n \n \n");
            
            return Ok(specialization);
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
    // [Authorize(Roles = "Admin")]
    public async Task<IActionResult> ListAllSpecializations()
    {
        Console.WriteLine("\n \n \n");
        Console.WriteLine("ENTROU AQUI222222222222: - ");
        Console.WriteLine("\n \n \n");
        
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
}