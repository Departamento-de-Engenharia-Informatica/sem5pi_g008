import {of, throwError} from 'rxjs';
import {AllergyService} from '../../../services/AllergyService/allergyService';
import {ComponentFixture, TestBed} from '@angular/core/testing';
import {Router} from '@angular/router';
import {EditAllergyComponent} from './edit-allergy.component';
import {HttpClientTestingModule} from '@angular/common/http/testing';
import {HomeButtonComponent} from '../../Shared/home-button/home-button.component';
import {GoBackButtonComponent} from '../../Shared/go-back-button/go-back-button.component';
import {FormsModule} from '@angular/forms';

describe('EditAllergyComponent', () => {
  let component: EditAllergyComponent;
  let fixture: ComponentFixture<EditAllergyComponent>;
  let allergyService: jasmine.SpyObj<AllergyService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(() => {
    spyOnProperty(window.history, 'state').and.returnValue({
      allergy: {
        domainId: "1",
        designation: 'Allergy 1',
        description: 'Test allergy description',
        effects: ['Effect 1'],
        code: 'A1'
      }
    });

    const allergyServiceSpy = jasmine.createSpyObj('AllergyService', ['updateAllergyDesignation', 'updateAllergyDescription', 'updateAllergyEffects']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    TestBed.configureTestingModule({
      declarations: [ EditAllergyComponent, HomeButtonComponent, GoBackButtonComponent ],
      imports: [HttpClientTestingModule,FormsModule],
      providers: [
        { provide: AllergyService, useValue: allergyServiceSpy },
        { provide: Router, useValue: routerSpy }
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(EditAllergyComponent);
    component = fixture.componentInstance;
    allergyService = TestBed.inject(AllergyService) as jasmine.SpyObj<AllergyService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should update allergy designation', async () => {
    allergyService.updateAllergyDesignation.and.returnValue(of(void 0));

    await component.updateAllergyDesignation('Updated Designation');
    expect(allergyService.updateAllergyDesignation).toHaveBeenCalledWith("1", 'Updated Designation');
  });

  it('should update allergy description', async () => {
    allergyService.updateAllergyDescription.and.returnValue(of(void 0));

    await component.updateAllergyDescription('Updated Description');
    expect(allergyService.updateAllergyDescription).toHaveBeenCalledWith("1", 'Updated Description');
  });

  it('should update allergy effects', async () => {
    allergyService.updateAllergyEffects.and.returnValue(of(void 0));

    await component.updateAllergyEffects(['Updated Effect']);
    expect(allergyService.updateAllergyEffects).toHaveBeenCalledWith("1", ['Updated Effect']);
  });

  it('should navigate to allergy management on successful update', async () => {
    allergyService.updateAllergyDesignation.and.returnValue(of(void 0));
    allergyService.updateAllergyDescription.and.returnValue(of(void 0));
    allergyService.updateAllergyEffects.and.returnValue(of(void 0));

    await component.editAllergy();
    expect(router.navigate).toHaveBeenCalledWith(['/admin/allergyManagement']);
    expect(component.hasError).toBeFalse();
  });
});
