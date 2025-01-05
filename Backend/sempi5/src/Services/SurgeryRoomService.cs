using Sempi5.Domain.AgendaAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SurgeryRoomAggregate;
using Sempi5.Infrastructure.SurgeryRoomAggregate;

namespace Sempi5.Services;

public class SurgeryRoomService
{
    private readonly ISurgeryRoomRepository _surgeryRoomRepository;
    private readonly IRoomAgendaRepository _roomAgendaRepository;
    private readonly IUnitOfWork _unitOfWork;

    public SurgeryRoomService(ISurgeryRoomRepository surgeryRoomRepository, IRoomAgendaRepository roomAgendaRepository,
        IUnitOfWork unitOfWork)
    {
        _surgeryRoomRepository = surgeryRoomRepository;
        _roomAgendaRepository = roomAgendaRepository;
        _unitOfWork = unitOfWork;
    }

    public async Task<List<bool>> getAllSurgeryRoomsOccupancy()
    {
        var surgeryRooms = await _surgeryRoomRepository.GetAllAsync();
        var occupancy = new List<bool>();
        foreach (var room in surgeryRooms)
        {
            var roomAgenda = await getRoomSchedule(room);
            Console.WriteLine(room.Id);
            Console.WriteLine(roomAgenda);
            Console.WriteLine(DateTime.Now);
            if(roomAgenda == "No schedule for the day")
            {
                occupancy.Add(false);
            }
            else
            {
                var timeIntervals = roomAgenda.Split(',');

                var foundTime = false;
                foreach (var interval in timeIntervals)
                {
                    var timeRange = interval.Split('-');
                    if (timeRange.Length == 2)
                    {
                        var startTime = DateTime.Parse(timeRange[0]);
                        var endTime = DateTime.Parse(timeRange[1]);
                        if (DateTime.Now >= startTime && DateTime.Now < endTime)
                        {
                            foundTime = true;
                        }
                    }
                    else
                    {
                        foundTime = false; 
                    }
                }
                occupancy.Add(foundTime);
            }
        }

        return occupancy;
    }

    public async Task<List<SurgeryRoom>> getSurgeryRoom()
    {
        return await _surgeryRoomRepository.GetAllStaff();
    }

    public async Task<List<List<Dictionary<string, string>>>> getRoomsInfo()
    {
        var surgeryRooms = await _surgeryRoomRepository.GetAllAsync();
        var dummyData = new List<List<Dictionary<string, string>>>();
        
        foreach (var surgeryRoom in surgeryRooms)
        {
            var roomAgenda = await getRoomSchedule(surgeryRoom);
            var roomData = new List<Dictionary<string, string>>
            {
                new Dictionary<string, string>
                {
                    { "title", "Room Type" },
                    { "body", surgeryRoom.Type.ToString() }
                },
                new Dictionary<string, string>
                {
                    { "title", "Room Schedule for the day" },
                    { "body", roomAgenda }
                }
            };

            dummyData.Add(roomData);
        }

        return dummyData;
    }

    private async Task<string> getRoomSchedule(SurgeryRoom surgeryRoom)
    {
        var agendaId = new AgendaId(surgeryRoom.Id.AsInt());
        var (year, month, day) = DateTime.Now;
        var agendas = await _roomAgendaRepository.GetRoomAgendasByRoomIdAndDay(agendaId, new DateTime(year, month, day));

        if (agendas.Count == 0 || agendas == null)
        {
            return "No schedule for the day";
        }
        
        var lastAgenda = agendas.Last();

        var agendaAsStr = "";
        lastAgenda.TimeIntervals.ForEach(interval => agendaAsStr += interval + ",");
        agendaAsStr = agendaAsStr.Remove(agendaAsStr.Length - 1);
        return agendaAsStr;
    }
}