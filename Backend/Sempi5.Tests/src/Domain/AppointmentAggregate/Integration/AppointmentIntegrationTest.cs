using System;
using System.Collections.Generic;
using Sempi5.Domain.AppointmentAggregate;
using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.SurgeryRoomAggregate;
using Sempi5.Domain.User;
using Xunit;

namespace Sempi5.Tests.Domain.AppointmentAggregate.Unit;

public class AppointmentIntegrationTest
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

    private OperationRequest CreateOperationRequest()
    {
        var doctor = CreateStaff();
        var patient = CreatePatient();
        var operationType = CreateOperationType();
        var deadLineDate = DateTime.Now;
        var priorityEnum = PriorityEnum.HIGH;
        return new OperationRequest(doctor, patient, operationType, deadLineDate, priorityEnum);
    }

    private SurgeryRoom CreateSurgeryRoom()
    {
        return new SurgeryRoom(RoomTypeEnum.RECOVERY_ROOM, new RoomCapacity(2), 
            new List<string> { "equipamento1", "equipamento2" }, 
            RoomStatusEnum.AVAILABLE, new List<string> { "maintenanceSlots" });
    }

    [Fact]
    public void CreateAppointmentWithValidParameters()
    {
        var operationRequest = CreateOperationRequest();
        var surgeryRoom = CreateSurgeryRoom();
        var date = DateTime.Now;
        var status = StatusEnum.SCHEDULED;

        var appointment = new Appointment(operationRequest, surgeryRoom, date, status);

        Assert.Equal(operationRequest, appointment.OperationRequest);
        Assert.Equal(surgeryRoom, appointment.SurgeryRoom);
        Assert.Equal(date, appointment.Date);
        Assert.Equal(status, appointment.Status);
    }

    [Fact]
    public void CreateAppointmentWithNullOperationRequest()
    {
        var surgeryRoom = CreateSurgeryRoom();
        var date = DateTime.Now;
        var status = StatusEnum.SCHEDULED;

        Assert.Throws<ArgumentNullException>(() => new Appointment(null, surgeryRoom, date, status));
    }

    [Fact]
    public void CreateAppointmentWithNullSurgeryRoom()
    {
        var operationRequest = CreateOperationRequest();
        var date = DateTime.Now;
        var status = StatusEnum.SCHEDULED;

        Assert.Throws<ArgumentNullException>(() => new Appointment(operationRequest, null, date, status));
    }
}
