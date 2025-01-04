import { Component, OnInit } from '@angular/core';
import { MedicalRecordMedicalConditionService } from '../../../../services/MedicalRecordMedicalConditionService/medical-record-medical-condition.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-update-medical-recor-conditions',
  templateUrl: './update-medical-recor-conditions.component.html',
  styleUrls: ['./update-medical-recor-conditions.component.css']
})
export class UpdateMedicalRecorConditionsComponent implements OnInit {
  medicalRecordConditions: any[] = []; // Lista de condições médicas
  selectedConditions: Set<any> = new Set(); // Conjunto para rastrear condições selecionadas
  errorMessage: string = ''; // Mensagem de erro (se houver)

  constructor(
    private medicalRecordConditionService: MedicalRecordMedicalConditionService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadMedicalConditions();
  }

  // Carregar condições médicas
  loadMedicalConditions(): void {
    this.medicalRecordConditionService.listMedicalRecordConditions().subscribe(
      (response) => {
        this.medicalRecordConditions = response.medicalRecordConditions;
console.log('Medical response response loaded:', response);
        console.log('Medical record conditions loaded:', this.medicalRecordConditions);
      },
      (error) => {
        this.errorMessage = 'Failed to load medical record conditions';
        console.error(this.errorMessage, error);
      }
    );
  }

  // Alternar seleção de uma condição
  toggleSelection(condition: any): void {
    if (this.selectedConditions.has(condition)) {
      this.selectedConditions.delete(condition);
    } else {
      this.selectedConditions.add(condition);
    }
  }

  // Verifica se uma condição está selecionada
  isSelected(condition: any): boolean {
    return this.selectedConditions.has(condition);
  }

  // Salvar condições selecionadas
  saveSelected(): void {
    const selectedArray = Array.from(this.selectedConditions);
    console.log('Selected conditions:', selectedArray);
    this.medicalRecordConditionService.saveSelectedMedicalRecordConditions(selectedArray).subscribe(
      (response) => {
        console.log('Conditions saved successfully:', response);
      },
      (error) => {
        console.error('Failed to save selected conditions:', error);
      }
    );
  }

  // Cancelar e redirecionar
  cancel(): void {
    this.router.navigate(['/']); // Redireciona para a página inicial ou outra rota
  }
}
