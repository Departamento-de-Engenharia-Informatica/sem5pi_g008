import {Component, Inject, OnInit} from '@angular/core';
import {SpecializationDTO} from '../../../DTO/SpecializationDTO';
import {SpecializationService} from '../../../services/SpecializationService/specialization.service';
import {Router} from '@angular/router';
import {CreateStaff} from '../../../Domain/CreateStaff';

@Component({
  selector: 'app-create-specialization',
  templateUrl: './create-specialization.component.html',
  styleUrl: './create-specialization.component.css'
})
export class CreateSpecializationComponent  implements OnInit {

  private specializationService: SpecializationService;
  private router: Router;

  public newSpecialization!: SpecializationDTO;
  public errorMessage: string = '';

  constructor(@Inject(SpecializationService) specializationService: SpecializationService, @Inject(Router) router: Router) {
    this.specializationService = specializationService;
    this.router = router;
  }

  ngOnInit() {

    this.newSpecialization = {} as SpecializationDTO;

    this.newSpecialization.specializationName = '';
  }

  public isFieldFilled(): boolean {

    return this.newSpecialization?.specializationName?.trim().length > 0;

    }


  public createSpecialization() {
    this.specializationService.createSpecialization(this.newSpecialization).subscribe(
      () => {
        this.router.navigate(['/admin/specialization']).then(() => {
          window.location.reload();
        });
      },
      (error) => {

        if(error.status === 652) {

          this.errorMessage = 'Specialization with this name already exists.';

        } else {

          console.log('An error occurred while creating the specialization:', error);

        }

      }
    );
  }

}
