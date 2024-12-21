using System;
using System.Collections.Generic;
using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.User;
using Xunit;

namespace Sempi5.Tests.Domain.OperationRequestAggregate.Integration;

public class OperationRequestTest
{
    private SystemUser CreateSystemUser()
    {
        var email = new Email("rute@gmail.com");
        return new SystemUser(email, "roleTest");
    }

    private Person CreatePerson()
    {
        var firstName = new Name("Rute");
        var lastName = new Name("Silva");
        var email = new Email("rui@teste.com");
        var phoneNumber = new PhoneNumber(987654321);
        var contactInfo = new ContactInfo(email, phoneNumber);
        return new Person(firstName, lastName, contactInfo);
    }
    
    private Specialization CreateSpecialization()
    {
        var name = new SpecializationName("Cardiologia");
        var code = new SpecializationCode("C1");
        var description = new SpecializationDescription("Especialização em Cardiologia");
        return new Specialization(name, code, description);
    }

    private Staff CreateStaff()
    {
        var licenseNumber = new LicenseNumber(123);
        var person = CreatePerson(); 
        var specialization = CreateSpecialization();
        return new Staff(licenseNumber, person, specialization);
    }
    
    private Patient CreatePatient()
    {
        var user = CreateSystemUser();
        var person = CreatePerson();
        return new Patient(user, person, DateTime.Now, "M", new List<string>(), "123456789", new List<string>());
    }
    
    private OperationType CreateOperationType()
    {
        var name = new OperationName("Cirurgia");
        return new OperationType(name, TimeSpan.FromHours(2), TimeSpan.FromHours(4), TimeSpan.FromHours(6));
    }
    
    [Fact]
    public void TestValidConstructor()
    {
        var doctor = CreateStaff();
        var patient = CreatePatient();
        var operationType = CreateOperationType();
        var deadLineDate = DateTime.Now;
        var priorityEnum = PriorityEnum.HIGH;
        
        var operationRequest = new OperationRequest(doctor, patient, operationType, deadLineDate, priorityEnum);
        
        Assert.NotNull(operationRequest);
    }
    
    [Fact]
    public void TestInvalidConstructorWithNullStaff()
    {
        Staff doctor = null;
        var patient = CreatePatient();
        var operationType = CreateOperationType();
        var deadLineDate = DateTime.Now;
        var priorityEnum = PriorityEnum.HIGH;
        
        Assert.Throws<ArgumentNullException>(() => new OperationRequest(doctor, patient, operationType, deadLineDate, priorityEnum));
    }
    
    [Fact]
    public void TestInvalidConstructorWithNullPatient()
    {
        var doctor = CreateStaff();
        Patient patient = null;
        var operationType = CreateOperationType();
        var deadLineDate = DateTime.Now;
        var priorityEnum = PriorityEnum.HIGH;
        
        Assert.Throws<ArgumentNullException>(() => new OperationRequest(doctor, patient, operationType, deadLineDate, priorityEnum));
    }
    
    [Fact]
    public void TestInvalidConstructorWithNullOperationType()
    {
        var doctor = CreateStaff();
        var patient = CreatePatient();
        OperationType operationType = null;
        var deadLineDate = DateTime.Now;
        var priorityEnum = PriorityEnum.HIGH;
        
        Assert.Throws<ArgumentNullException>(() => new OperationRequest(doctor, patient, operationType, deadLineDate, priorityEnum));
    }
}
