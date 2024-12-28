import {Component, Inject} from '@angular/core';
import {Allergy} from '../../../Domain/Allergy';
import {AllergyService} from '../../../services/AllergyService/allergyService';
import {DisplayAllergyDTO} from '../../../DTOs/displayDTOs/displayAllergyDTO';
import {AllergyMapper} from '../../../DTOs/mappers/allergyMapper';
import {Router} from '@angular/router';

@Component({
  selector: 'app-allergy-management',
  templateUrl: './allergy-management.component.html',
  styleUrl: './allergy-management.component.css'
})
export class AllergyManagementComponent {

  public allergies: DisplayAllergyDTO[] = [];
  private allergyService: AllergyService;

  constructor(@Inject(AllergyService) allergyService: AllergyService,private router: Router) {
    this.allergyService = allergyService;
  }

  ngOnInit() {
    this.loadAllergies();
  }

  private loadAllergies() {
    this.allergyService.getAllAllergies().subscribe(
      (allergies) => {
        console.log('Allergies:', allergies);
        for(let allergy of allergies.allergies) {
          this.allergies.push(AllergyMapper.domainToDisplayDto(allergy));
        }
      },
      (error) => {
        console.error('Failed to load allergies:', error);
      }
    );
  }

  public addAllergy() {
    this.router.navigate(['admin/allergyManagement/add']);
  }

  public editAllergy(allergy: DisplayAllergyDTO) {
    this.router.navigate(['admin/allergyManagement/edit'], { state : {allergy : allergy }});
  }
}
