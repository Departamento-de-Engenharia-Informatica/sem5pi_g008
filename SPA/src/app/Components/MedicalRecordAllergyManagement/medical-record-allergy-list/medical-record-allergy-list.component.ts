import {Component, Inject, Input, OnInit} from "@angular/core";
import {
  MedicalRecordAllergyService
} from "../../../services/medicalRecordAllergyService/medical-record-allergy.service";
import {DisplayMedicalRecordAllergyDTO} from "../../../DTOs/displayDTOs/displayMedicalRecordAllergyDTO";
import {Router} from '@angular/router';

@Component({
  selector: "app-medical-record-allergy-list",
  templateUrl: "./medical-record-allergy-list.component.html",
  styleUrls: ["./medical-record-allergy-list.component.css"],
})
export class MedicalRecordAllergyListComponent implements OnInit {
  constructor(
    @Inject(MedicalRecordAllergyService)
    private medicalRecordAllergyService: MedicalRecordAllergyService, private router: Router
  ) {}

  public errorMessage: string = "";
  public medicalRecordAllergies: DisplayMedicalRecordAllergyDTO[] = [];
  public filteredAllergies: DisplayMedicalRecordAllergyDTO[] = [];
  @Input() public medicalRecordId: string = "";
  public searchQuery: string = "";

  ngOnInit(): void {
    this.fetchMedicalRecordConditions();
  }

  fetchMedicalRecordConditions(): void {
    this.medicalRecordAllergyService
      .listAllergiesInMedicalRecord(this.medicalRecordId)
      .subscribe(
        (response) => {
          if (response && response.length > 0) {
            this.medicalRecordAllergies = response;
            this.filteredAllergies = [...this.medicalRecordAllergies];
            this.errorMessage = "";
          } else {
            this.errorMessage = "No allergies found.";
            this.filteredAllergies = [];
          }
        },
        (error) => {
          this.errorMessage = "Failed to load allergies.";
          console.error("Failed to load allergies:", error);
        }
      );
  }

  searchAllergy(): void {
    if (this.searchQuery.trim() === "") {
      this.filteredAllergies = [...this.medicalRecordAllergies];
      this.errorMessage = "";
    } else {
      this.filteredAllergies = this.medicalRecordAllergies.filter((allergy) =>
        allergy.allergy
          .toLowerCase()
          .includes(this.searchQuery.toLowerCase().trim())
      );

      if (this.filteredAllergies.length === 0) {
        this.errorMessage = `No allergies found for "${this.searchQuery}".`;
      } else {
        this.errorMessage = "";
      }
    }
  }

  redirectToEdit(domainId: string | undefined) {
    console.log("domainId in list",domainId);
    this.router.navigate(['staff/patients/medicalRecord/editAllergy'], {
      queryParams: { id: domainId }
    });
  }

}
