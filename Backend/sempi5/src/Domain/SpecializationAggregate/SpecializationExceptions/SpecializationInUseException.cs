namespace Sempi5.Domain.SpecializationAggregate.SpecializationExceptions;

public class SpecializationInUseException : Exception
{
    public int StatusCode { get; set; } = 651;

    public SpecializationInUseException(string message) : base(message)
    {
    }
}