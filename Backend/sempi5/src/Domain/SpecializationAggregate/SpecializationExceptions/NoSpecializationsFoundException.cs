namespace Sempi5.Domain.SpecializationAggregate.SpecializationExceptions;

public class NoSpecializationsFoundException : Exception
{
    public int StatusCode { get; set; } = 650;
    
    public NoSpecializationsFoundException(string message) : base(message)
    {
    }
}
