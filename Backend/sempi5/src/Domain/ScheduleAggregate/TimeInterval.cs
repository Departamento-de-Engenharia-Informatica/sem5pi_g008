using System.Runtime.InteropServices.JavaScript;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;

namespace Sempi5.Domain;

public class TimeInterval
{
    public DateTime IntervalStart { get; private set; }
    public DateTime IntervalEnd { get; private set; }

    // Para EF Core
    private TimeInterval() { }
}