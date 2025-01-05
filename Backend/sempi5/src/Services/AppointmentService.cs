using Sempi5.Domain.AppointmentAggregate;
using Sempi5.Domain.AppointmentAggregate.DTO;
using Sempi5.Infrastructure.AppointmentAggregate;
using Sempi5.Infrastructure.OperationRequestAggregate;
using Sempi5.Infrastructure.SurgeryRoomAggregate;

namespace Sempi5.Services;

public class AppointmentService
{
    private readonly IAppointmentRepository _appointmentRepository;
    private readonly IOperationRequestRepository _operationRequestRepository;
    private readonly ISurgeryRoomRepository _surgeryRoomRepository;

    public AppointmentService(IAppointmentRepository appointmentRepository,
        IOperationRequestRepository operationRequestRepository, ISurgeryRoomRepository surgeryRoomRepository)
    {
        _appointmentRepository = appointmentRepository;
        _operationRequestRepository = operationRequestRepository;
        _surgeryRoomRepository = surgeryRoomRepository;
    }

    public async Task createAppointment(AppointmentDto appointmentDto)
    {
        if (appointmentDto.operationRequestId != null)
        {
            var operationRequest =
                await _operationRequestRepository.GetOperationRequestById(appointmentDto.operationRequestId);
            int roomID = int.Parse(appointmentDto.surgeryRoomId);
            var surgeryRoom = await _surgeryRoomRepository.GetSurgeryRoomById(roomID);
            DateTime date = DateTime.Parse(appointmentDto.surgeryDate);
            var appointment = new Appointment(operationRequest, surgeryRoom, date, StatusEnum.SCHEDULED);
            await _appointmentRepository.addAppointment(appointment);
        }
    }

    public async Task updateAppointment(AppointmentDto appointmentDto)
    {
        //alterar data,status, sala
        int appointmentId = int.Parse(appointmentDto.id);
        var appointment = await _appointmentRepository.GetAppointmentById(appointmentId);
        if (appointment != null)
        {
            if (appointmentDto.status != null) appointment.Status = Enum.Parse<StatusEnum>(appointmentDto.status, true);
            if (appointmentDto.surgeryDate != null) appointment.Date = DateTime.Parse(appointmentDto.surgeryDate);
            var surgeryRoom = await _surgeryRoomRepository.GetSurgeryRoomById(int.Parse(appointmentDto.surgeryRoomId));
            if (appointmentDto.surgeryRoomId != null) appointment.SurgeryRoom = surgeryRoom;
            await _appointmentRepository.updataAppointment(appointment);
        }
    }

    public async Task<List<AppointmentDto>> getAppointments()
    {
        var appointmentDtoList = new List<AppointmentDto>();
        var appointment = await _appointmentRepository.GetAllAppointmentsAsync();
        for (int i = 0; i < appointment.Count; i++)
        {
            var appointmentDto = new AppointmentDto();
            appointmentDto.id = appointment[i].Id.AsString();
            appointmentDto.operationRequestId = appointment[i].OperationRequest.Id.AsString();
            appointmentDto.surgeryRoomId = appointment[i].SurgeryRoom.Id.AsString();
            appointmentDto.surgeryDate = appointment[i].Date.ToString();
            appointmentDto.status = appointment[i].Status.ToString();
            appointmentDtoList.Add(appointmentDto);
        }

        return appointmentDtoList;
    }
}