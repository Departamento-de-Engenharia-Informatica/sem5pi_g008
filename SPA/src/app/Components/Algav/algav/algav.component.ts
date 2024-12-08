import { Component, OnInit } from '@angular/core';
import { SurgeryRoomService } from '../../../services/SurgeryRoomService/surgery-room.service';

@Component({
  selector: 'app-algav',
  templateUrl: './algav.component.html',
  styleUrls: ['./algav.component.css']
})
export class AlgavComponent implements OnInit {
  surgeryRooms: any[] = [];
  surgeryPlan: any = {}; // Para armazenar os dados do plano de cirurgia

  constructor(private surgeryService: SurgeryRoomService) {}

  ngOnInit(): void {
    this.surgeryService.getSurgeryPlan().subscribe({
      next: (data) => {
        console.log('Surgery Plan:', data);
        this.surgeryPlan = data;  // Armazena os dados do plano de cirurgia
      },
      error: (err) => {
        console.error('Error fetching data:', err);
      }
    });
  }
}
