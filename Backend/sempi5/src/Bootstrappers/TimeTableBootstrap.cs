using Sempi5.Domain;
using Sempi5.Domain.AgendaAggregate;
using Sempi5.Domain.PersonalData;

namespace Sempi5.Bootstrappers;

public class TimeTableBootstrap
{
    private readonly IRoomAgendaRepository _surgeryRoomRepository;
    private readonly IStaffAgendaRepository _staffAgendaRepository;
 
    
    public TimeTableBootstrap( IStaffAgendaRepository staffAgendaRepository, IRoomAgendaRepository surgeryRoomRepository)
    {
        _staffAgendaRepository = staffAgendaRepository;
        _surgeryRoomRepository = surgeryRoomRepository;
    }


    public async Task SeeedTavbeTime()
    {
        // Example data for the RoomAgenda
        var agenda1 = new RoomAgenda
        {
            Date = new DateTime(2024, 12, 15),
            TimeIntervals = new List<string> { "09:00-10:00", "10:00-11:00", "11:00-12:00" }
        };

        var agenda2 = new RoomAgenda
        {
            Date = new DateTime(2024, 12, 16),
            TimeIntervals = new List<string> { "09:00-10:00", "10:00-11:00", "11:00-12:00" }
        };

        // Insert into database (assuming _context is already set up)
        
        await _surgeryRoomRepository.AddAsync(agenda1);
        await _surgeryRoomRepository.AddAsync(agenda2);


    }

}