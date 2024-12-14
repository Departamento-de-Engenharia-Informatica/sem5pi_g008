using Sempi5.Domain;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.SurgeryRoomAggregate;
using Sempi5.Infrastructure.OperationTypeAggregate;
using Sempi5.Infrastructure.SurgeryRoomAggregate;
using OperationType = Microsoft.OpenApi.Models.OperationType;

namespace Sempi5.Bootstrappers;

public class SurgeryRoomBootstrap
{
    private readonly ISurgeryRoomRepository _surgeryRoomRepository;

    public SurgeryRoomBootstrap(ISurgeryRoomRepository surgeryRoomRepository)
    {
        _surgeryRoomRepository = surgeryRoomRepository;
    }

    public async Task SeedSurgeryRooms()
    {
        var agenda1 = new RoomAgenda
        {
            Date = new DateTime(2024, 12, 16),
            TimeIntervals = new List<string> { "09:00-10:00", "10:00-11:00", "11:00-12:00" },
            appointmentType = "Surgery"
        };

        var agenda2 = new RoomAgenda
        {
            Date = new DateTime(2024, 12, 16),
            TimeIntervals = new List<string> { "09:00-10:00", "10:00-11:00", "11:00-12:00" },
            appointmentType = "Surgery"
            
                
        };
        var surgeryRoom1 = new SurgeryRoom
        (
            RoomTypeEnum.OPERATING_ROOM,
            new RoomCapacity(1),
            ["Surgical Table", "Surgical Light"],
            RoomStatusEnum.AVAILABLE,
            [],
            new List<RoomAgenda> { agenda1, agenda2 }
        );
        
        var surgeryRoom2 = new SurgeryRoom
        (
            RoomTypeEnum.OPERATING_ROOM,
            new RoomCapacity(2),
            ["Surgical Table", "Surgical Light"],
            RoomStatusEnum.AVAILABLE,
            []
            ,            new List<RoomAgenda> { agenda1, agenda2 }

        );
        
        var surgeryRoom3 = new SurgeryRoom
        (
            RoomTypeEnum.OPERATING_ROOM,
            new RoomCapacity(2),
            ["Surgical Table", "Surgical Light"],
            RoomStatusEnum.AVAILABLE,
            []            ,            new List<RoomAgenda> { agenda1, agenda2 }

        );
        
        var surgeryRoom4 = new SurgeryRoom
        (
            RoomTypeEnum.OPERATING_ROOM,
            new RoomCapacity(2),
            ["Surgical Table", "Surgical Light"],
            RoomStatusEnum.AVAILABLE,
            []            ,            new List<RoomAgenda> { agenda1 }

        );
        
        var surgeryRoom5 = new SurgeryRoom
        (
            RoomTypeEnum.OPERATING_ROOM,
            new RoomCapacity(2),
            ["Surgical Table", "Surgical Light"],
            RoomStatusEnum.AVAILABLE,
            []            ,            new List<RoomAgenda> { agenda1 }

        );
        
        var surgeryRoom6 = new SurgeryRoom
        (
            RoomTypeEnum.OPERATING_ROOM,
            new RoomCapacity(2),
            ["Surgical Table", "Surgical Light"],
            RoomStatusEnum.AVAILABLE,
            []            ,            new List<RoomAgenda> {  agenda2 }

        );

        await _surgeryRoomRepository.AddAsync(surgeryRoom1);
        await _surgeryRoomRepository.AddAsync(surgeryRoom2);
        await _surgeryRoomRepository.AddAsync(surgeryRoom3);
        await _surgeryRoomRepository.AddAsync(surgeryRoom4);
        await _surgeryRoomRepository.AddAsync(surgeryRoom5);
        await _surgeryRoomRepository.AddAsync(surgeryRoom6);
    }
}