﻿using Sempi5.Domain.AppointmentAggregate;
using Sempi5.Domain.Shared;

namespace Sempi5.Infrastructure.AppointmentAggregate;

public interface IAppointmentRepository : IRepository<Appointment, AppointmentID>
{
    Task SaveChangesAsync();
    Task<Appointment?> getAppointmentByOperationRequestID(long id);
    Task updataAppointment(Appointment appointment);
}