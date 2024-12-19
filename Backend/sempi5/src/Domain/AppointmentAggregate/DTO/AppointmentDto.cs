namespace Sempi5.Domain.AppointmentAggregate.DTO;

public class AppointmentDto
{
    public string? id { get; set; } 

    public string? operationRequestId { get; set; } 

    public string? surgeryRoomId { get; set; } 

    public string? surgeryDate { get; set; } 

    public string? status { get; set; } 
}