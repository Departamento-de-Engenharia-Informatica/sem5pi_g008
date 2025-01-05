using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Sempi5.Services;

namespace Sempi5.Controllers;

[Route("[controller]")]
[ApiController]
public class SurgeryRoomController : ControllerBase
{
    private readonly SurgeryRoomService _surgeryRoomService;
    
    public SurgeryRoomController(SurgeryRoomService surgeryRoomService)
    {
        _surgeryRoomService = surgeryRoomService;
    }
    
    [Authorize(Policy = "Staff")]
    [HttpGet("status")]
    public async Task<IActionResult> GetSurgeryRooms()
    {
        try
        {
            var surgeryRooms = await _surgeryRoomService.getAllSurgeryRoomsOccupancy();
            return Ok(surgeryRooms);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }
    
    [Authorize(Policy = "Staff")]
    [HttpGet("info")]
    public async Task<IActionResult> GetRoomsInfo()
    {
        try
        {
            var roomsInfo = await _surgeryRoomService.getRoomsInfo();
            return Ok(roomsInfo);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }
}