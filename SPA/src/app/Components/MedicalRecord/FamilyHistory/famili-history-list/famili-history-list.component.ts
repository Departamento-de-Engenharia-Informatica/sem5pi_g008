import { Component, Input, OnInit } from '@angular/core';
import { FamilyHistoryService } from '../../../../services/MedicalRecordFamilyHistory/family-history.service';

@Component({
  selector: 'app-famili-history-list',
  templateUrl: './famili-history-list.component.html',
  styleUrls: ['./famili-history-list.component.css']
})
export class FamiliHistoryListComponent implements OnInit {
  @Input() public medicalRecordId: string = "";
  public familyHistoryData: any[] = [];

  constructor(private familyHistoryService: FamilyHistoryService) {}

  ngOnInit(): void {
    // Carregar dados automaticamente ao inicializar o componente
    this.fetchFamilyHistory();
  }

  fetchFamilyHistory(): void {
    if (this.medicalRecordId) {
      this.familyHistoryService.getMedicalRecordFamilyHistoryWithIds(this.medicalRecordId)
        .subscribe(
          (response: any) => {
            this.familyHistoryData = response.medicalRecordFamilyHistory;
          },
          (error) => {
            console.error('Falha ao carregar histórico familiar:', error);
          }
        );
    }
  }
}
