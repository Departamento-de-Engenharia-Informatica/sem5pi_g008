namespace Sempi5.Domain.OperationRequestAggregate.DTOs;

public class OperationRequestDataDto
{
    public string? operationRequestId { get; set; }
    public  string? patientName { get; set; } 
    public  string? operationName { get; set; }
    public  string? operationType { get; set; }
    public  string? deadline{ get; set; }
    public  string? priority{ get; set; }
    public  string? status{ get; set; }
}