import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { AppointementService } from '../../../services/AppointmentService/appointement.service';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { AppointmentDTO } from '../../../DTOs/AppointmentDto/AppointmentDto';

@Component({
  selector: 'app-update-appointment',
  templateUrl: './update-appointment.component.html',
  styleUrls: ['./update-appointment.component.css'],
  standalone: false
})
export class UpdateAppointmentComponent implements OnInit {
  registrationForm!: FormGroup;
  appointments: AppointmentDTO[] = [];  // List of appointments fetched
  appointmentDTO!: AppointmentDTO;     // The selected appointment
  errorMessage!: string;               // Error message variable

  constructor(
    private router: Router,
    private appointmentService: AppointementService,
    private fb: FormBuilder
  ) {
    this.registrationForm = this.fb.group({
      surgeryDate: ['', Validators.required],
      surgeryRoom: ['', Validators.required]
    });
  }

  ngOnInit(): void {
    this.displayAppointmentDetails();
  }

  displayAppointmentDetails() {
    console.log('Fetching appointment details...');
    this.appointmentService.getAppointments().subscribe(
      (response: AppointmentDTO[]) => {
        console.log('Response:', response);
        this.appointments = response;
      },
      (error: any) => {
        this.errorMessage = error || 'An unknown error occurred.';
      }
    );
  }

  onAppointmentSelect(appointment: AppointmentDTO) {
    this.appointmentDTO = appointment;
    this.registrationForm.patchValue({
      surgeryDate: appointment.surgeryDate,
      surgeryRoom: appointment.surgeryRoomId
    });
  }

  updateAppointment() {
    if (this.registrationForm.valid) {
      this.appointmentDTO = {
        ...this.appointmentDTO,
        surgeryDate: this.registrationForm.get('surgeryDate')?.value,
        surgeryRoomId: this.registrationForm.get('surgeryRoom')?.value,
      };

      this.appointmentService.updateAppointment(this.appointmentDTO).subscribe(
        (response: any) => {
          alert('Appointment updated successfully!');
        },
        (error: any) => {
          alert(error || 'An unknown error occurred.');
        }
      );
    } else {
      alert('Please fill out all required fields.');
    }
  }
}
