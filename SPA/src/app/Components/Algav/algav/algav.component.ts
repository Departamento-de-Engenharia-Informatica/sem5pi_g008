import { Component, OnInit } from '@angular/core';
import { SurgeryRoomService } from '../../../services/SurgeryRoomService/surgery-room.service';

@Component({
  selector: 'app-algav',
  templateUrl: './algav.component.html',
  styleUrls: ['./algav.component.css']
})
export class AlgavComponent implements OnInit {
  surgeryPlan: any = {};
  agenda: any = {};

  formData = {
    date: '',
    room: ''
  };
  constructor(private surgeryService: SurgeryRoomService) {}

  ngOnInit(): void {
  //  this.getSurgeryPlan();
 //  this.getStaff();
 //   this.getSurgeryRoomsInfo();
 //   this.getRequests();
 //    this.getOperationTypes();
 //    this.getRequiredStaff();
  //  this.loadData();
  // this.getData();
  }

  // Método para obter o plano de cirurgia

  searchSurgeryPlan() {
    const { date, room } = this.formData;
    console.log('Date:', date);
    console.log('Room:', room);
    this.surgeryService.getSurgeryPlan(date, room).subscribe(data => {
      this.surgeryPlan = data;
      console.log('Surgery Plan:', this.surgeryPlan);
    });
  }

  loadData(): void {
    this.surgeryService.loadData().subscribe({
      next: (data) => {
        console.log('loadData:', data);
      },
      error: (err) => {
        console.error('Error fetching surgery rooms:', err);
      }
    });
  }

  // Método para obter a lista de salas de cirurgia
  getOperationTypes(): void {
    this.surgeryService.getOperationTypes().subscribe({
      next: (data) => {
        console.log('getOperationTypes:', data);
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
        console.log('Operation Request:', data);
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
      },
      error: (err) => {
        console.error('Error fetching surgery rooms:', err);
      }
    });
  }
  getData(): void {
    this.surgeryService.getData().subscribe({
      next: (data) => {
        console.log('getData:', data);
        this.agenda = data;
      },
      error: (err) => {
        console.error('Error fetching surgery rooms:', err);
      }
    });
  }
}
