using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.StaffAggregate.DTOs;

namespace Sempi5.Mappers;

public class StaffMapper
{
    public static StaffDetailsDTO toDTO(Staff staff)
    {
        return new StaffDetailsDTO
        {
            FirstName = staff.Person.FirstName._name,
            LastName = staff.Person.LastName._name,
            LicenseNumber = staff.LicenseNumber.licenseNumber()
        };
    }
}