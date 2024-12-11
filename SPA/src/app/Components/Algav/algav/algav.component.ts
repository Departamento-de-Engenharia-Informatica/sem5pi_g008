import { Component, OnInit } from '@angular/core';
import { SurgeryRoomService } from '../../../services/SurgeryRoomService/surgery-room.service';

@Component({
  selector: 'app-algav',
  templateUrl: './algav.component.html',
  styleUrls: ['./algav.component.css']
})
export class AlgavComponent implements OnInit {
  surgeryRooms: any[] = [];
  surgeryPlan: any = {};
  surgerySchedules: any[] = [];
  availableRooms: any[] = [];
  constructor(private surgeryService: SurgeryRoomService) {}

  ngOnInit(): void {
   // this.getSurgeryPlan();
   //  this.getStaff();
   // this.getSurgeryRoomsInfo();
    this.getRequests();
    // this.getOperationTypes();
    // this.getRequiredStaff();
  }

  // Método para obter o plano de cirurgia
  getSurgeryPlan(): void {
    this.surgeryService.getSurgeryPlan().subscribe({
      next: (data) => {
        console.log('Surgery Plan:', data);
        this.surgeryPlan = data;
      },
      error: (err) => {
        console.error('Error fetching surgery plan:', err);
      }
    });
  }

  // Método para obter a lista de salas de cirurgia
  getOperationTypes(): void {
    this.surgeryService.getOperationTypes().subscribe({
      next: (data) => {
        console.log('getOperationTypes:', data);
        this.surgeryRooms = data;
      },
      error: (err) => {
        console.error('Error fetching surgery rooms:', err);
      }
    });
  }

  // Método para obter agendamentos de cirurgia
  getRequests(): void {
    this.surgeryService.getRequests().subscribe({
      next: (data) => {
        console.log('Surgery Schedules:', data);
        this.surgerySchedules = data;
      },
      error: (err) => {
        console.error('Error fetching surgery schedules:', err);
      }
    });
  }

  // Método para obter salas disponíveis
  getSurgeryRoomsInfo(): void {
    this.surgeryService.getSurgeryRoomsInfo().subscribe({
      next: (data) => {
        console.log('Available Rooms:', data);
        this.availableRooms = data;
      },
      error: (err) => {
        console.error('Error fetching available rooms:', err);
      }
    });
  }
  // Método para obter a lista de salas de cirurgia
  getStaff(): void {
    this.surgeryService.getStaff().subscribe({
      next: (data) => {
        console.log('getStaff:', data);
        this.surgeryRooms = data;
      },
      error: (err) => {
        console.error('Error fetching surgery rooms:', err);
      }
    });
  }
  getRequiredStaff(): void {
    this.surgeryService.getRequiredStaff().subscribe({
      next: (data) => {
        console.log('getRequiredStaff:', data);
        this.surgeryRooms = data;
      },
      error: (err) => {
        console.error('Error fetching surgery rooms:', err);
      }
    });
  }
}
