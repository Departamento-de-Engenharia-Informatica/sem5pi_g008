import {Component, OnInit} from '@angular/core';
import {FormBuilder, FormGroup, Validators} from '@angular/forms';
import {ActivatedRoute} from '@angular/router';
import {MedicalRecordAllergyService} from '../../../services/medicalRecordAllergyService/medical-record-allergy.service';


@Component({
  selector: 'app-update-medical-record-allergy',
  templateUrl: './update-medical-record-allergy.component.html',
  styleUrls: ['./update-medical-record-allergy.component.css']
})

export class UpdateMedicalRecordAllergyComponent implements OnInit{
  updateForm: FormGroup;
  allergyId!: string;

  constructor(
    private fb: FormBuilder,
    private medicalRecordAllergyService: MedicalRecordAllergyService,
    private route: ActivatedRoute
  ) {
    this.updateForm = this.fb.group({
      comment: ['', Validators.required]
    });
  }

  ngOnInit() {
    this.route.queryParams.subscribe(params => {
      this.allergyId = params['id'];
      console.log(this.allergyId);
    });
  }

  onSubmit(): void {
    if (this.updateForm.valid) {
      const { comment } = this.updateForm.value;

      this.medicalRecordAllergyService.updateComment(this.allergyId.toString(), comment).subscribe(
        () => {
          alert('Comment updated successfully!');
          this.updateForm.reset();
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
