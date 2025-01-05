import {Component, EventEmitter, Input, Output} from '@angular/core';
import {Specialization} from '../../../Domain/Specialization';
import {OperationTypeService} from '../../../services/OperationTypeService/operation-type.service';
import {Router} from '@angular/router';
import {SpecializationService} from '../../../services/SpecializationService/specialization.service';

@Component({
  selector: 'app-specialization-list',
  templateUrl: './specialization-list.component.html',
  styleUrl: './specialization-list.component.css'
})
export class SpecializationListComponent {
  @Input() specializationList: Specialization[] = [];
   @Output() deleteSpecialization = new EventEmitter<string>();
  // @Output() editSpecialization = new EventEmitter<Specialization>();

  constructor(private specializationService: SpecializationService, private router: Router) {
    this.specializationService = specializationService;
  }

  editSpecialization(specialization: Specialization) {
    this.router.navigate(["admin/specialization/edit"], {state: {specialization: specialization}});
  }

}
