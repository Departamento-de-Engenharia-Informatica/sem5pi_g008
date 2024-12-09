using Sempi5.Domain.Shared;
using Sempi5.Domain.SurgeryRoomAggregate;
using Sempi5.Infrastructure.SurgeryRoomAggregate;

namespace Sempi5.Services;

public class SurgeryRoomService
{
 private readonly ISurgeryRoomRepository _surgeryRoomRepository;
 private readonly IUnitOfWork _unitOfWork;
 
 public SurgeryRoomService(ISurgeryRoomRepository surgeryRoomRepository, IUnitOfWork unitOfWork)
 {
     _surgeryRoomRepository = surgeryRoomRepository;
     _unitOfWork = unitOfWork;
 }
 
 public async Task<List<bool>> getAllSurgeryRoomsOccupancy()
 {
     var surgeryRooms = await _surgeryRoomRepository.GetAllAsync();
     var occupancy = new List<bool>();
     foreach (var room in surgeryRooms)
     {
         occupancy.Add(room.Status == RoomStatusEnum.OCCUPIED);
     }
     return occupancy;
 }

 public async Task<List<SurgeryRoom>>getSurgeryRoom()
 {
    return await _surgeryRoomRepository.GetAllAsync();
 }
}