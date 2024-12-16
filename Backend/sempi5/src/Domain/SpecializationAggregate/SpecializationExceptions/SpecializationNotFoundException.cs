namespace Sempi5.Domain.SpecializationAggregate.SpecializationExceptions;

public class SpecializationNotFoundException : Exception
{
    
    public int StatusCode { get; set; } = 651;
    
    public SpecializationNotFoundException(string message) : base(message)
    {
    }
}