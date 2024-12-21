import {Component} from '@angular/core';
import {Router} from '@angular/router';
import {PatientProfileService} from '../../../../services/PatientProfileService/patient-profile-service';
import {AppointementService} from '../../../../services/AppointmentService/appointement.service';
import {FormBuilder, FormGroup, Validators} from '@angular/forms';
import {OperationRequestService} from '../../../../services/OperationRequestService/operation-request.service';

@Component({
  selector: 'app-create-appointment',
  templateUrl: './create-appointment.component.html',
  styleUrl: './create-appointment.component.css',
  standalone: false

})
export class CreateAppointmentComponent {
  registrationForm: FormGroup;
  operationRequestId: any;
  appointmentDTO: any = {
    operationRequestID: '',
    surgeryDate: '',
    surgeryRoom: '',
  }

  constructor(private requisitionService: OperationRequestService, private router: Router, private appointmentService: AppointementService, private fb: FormBuilder) {
    const navigation = this.router.getCurrentNavigation();
    this.operationRequestId = navigation?.extras?.state?.['operationRequestId'];
    this.appointmentDTO.operationRequestID = this.operationRequestId;

    console.log('Received operationRequestId:', this.operationRequestId);
    this.registrationForm = this.fb.group({
      surgeryDate: ['', Validators.required],
      surgeryRoom: ['', Validators.required],
    });
  }

  createAppointment() {
    if (this.registrationForm.valid) {
      this.appointmentDTO.surgeryDate = this.registrationForm.get('surgeryDate')?.value;
      this.appointmentDTO.surgeryRoom = this.registrationForm.get('surgeryRoom')?.value;
      console.log('Creating appointment:', this.appointmentDTO);
      this.appointmentService.createAppointment(this.appointmentDTO).subscribe(
        (response: any) => {
          alert('Appointment created successfully!');
        },
        (error: any) => {
          alert(error || 'An unknown error occurred.');
        }
      );
    }
  }

}
