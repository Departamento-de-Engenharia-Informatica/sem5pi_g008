import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import {
  MedicalRecordMedicalConditionService
} from '../../../services/MedicalRecordMedicalConditionService/medical-record-medical-condition.service';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-update-medical-record-condition',
  templateUrl: './update-medical-record-condition.component.html',
  styleUrls: ['./update-medical-record-condition.component.css']
})
export class UpdateMedicalRecordConditionComponent implements OnInit {
  updateForm: FormGroup;
  conditionId!: string; // Usando o operador de asserção

  constructor(
    private fb: FormBuilder,
    private medicalRecordConditionService: MedicalRecordMedicalConditionService,
    private route: ActivatedRoute
  ) {
    // Inicializando o formulário com validação para o comentário
    this.updateForm = this.fb.group({
      comment: ['', Validators.required]
    });
  }

  ngOnInit() {
    this.route.queryParams.subscribe(params => {
      this.conditionId = params['id'];
      console.log(this.conditionId);
    });
  }

  onSubmit(): void {
    if (this.updateForm.valid) {
      const { comment } = this.updateForm.value;

      // Enviando os dados para o serviço
      this.medicalRecordConditionService.updateComment(this.conditionId.toString(), comment).subscribe(
        () => {
          alert('Comment updated successfully!');
          this.updateForm.reset(); // Resetando o formulário
        },
        (error) => {
          console.error('Failed to update the comment:', error);
          alert('Error updating comment. Please try again.');
        }
      );
    } else {
      alert('Please fill out all required fields.');
    }
  }
}
