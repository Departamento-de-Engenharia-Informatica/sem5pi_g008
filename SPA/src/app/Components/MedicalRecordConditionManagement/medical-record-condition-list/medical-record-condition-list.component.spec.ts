import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MedicalRecordConditionListComponent } from './medical-record-condition-list.component';
import { MedicalRecordMedicalConditionService } from '../../../services/MedicalRecordMedicalConditionService/medical-record-medical-condition.service';
import { of, throwError } from 'rxjs';
import { EnterFilterNameComponent } from '../../Shared/enter-filter-name/enter-filter-name.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('MedicalRecordConditionListComponent', () => {
  let component: MedicalRecordConditionListComponent;
  let fixture: ComponentFixture<MedicalRecordConditionListComponent>;
  let mockMedicalRecordConditionService: jasmine.SpyObj<MedicalRecordMedicalConditionService>;

  beforeEach(() => {
    mockMedicalRecordConditionService = jasmine.createSpyObj(
      'MedicalRecordMedicalConditionService', [
      'listMedicalRecordConditionsByMedicalRecordId',
      'filterMedicalRecordConditionsByCode',
      'filterMedicalRecordConditionsByDesignation',
    ]);

    TestBed.configureTestingModule({
      declarations: [MedicalRecordConditionListComponent, EnterFilterNameComponent],
      imports: [HttpClientTestingModule],
      providers: [
        { provide: MedicalRecordMedicalConditionService, useValue: mockMedicalRecordConditionService },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(MedicalRecordConditionListComponent);
    component = fixture.componentInstance;
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  describe('fetchMedicalRecordConditions', () => {
    it('should fetch and populate medical record conditions', () => {
      const mockConditions = [
        {
          id: '1',
          conditionCode: 'C1',
          conditionDesignation: 'Test Designation',
          doctorName: 'Dr. Smith',
          comment: 'Test Comment',
        },
      ];
      mockMedicalRecordConditionService.listMedicalRecordConditionsByMedicalRecordId.and.returnValue(of({ medicalRecordConditions: mockConditions }));

      component.fetchMedicalRecordConditions();

      expect(mockMedicalRecordConditionService.listMedicalRecordConditionsByMedicalRecordId).toHaveBeenCalledWith(component.medicalRecordId);
      expect(component.medicalRecordConditions).toEqual(mockConditions);
    });

    it('should handle errors during fetch', () => {
      mockMedicalRecordConditionService.listMedicalRecordConditionsByMedicalRecordId.and.returnValue(throwError('Error fetching conditions'));

      component.fetchMedicalRecordConditions();

      expect(component.errorMessage).toBe('Error fetching conditions');
    });
  });

  describe('handleEnterFilterValue', () => {
    it('should apply code filter', () => {
      const filterValue = 'C1';
      component.currentFilter = 'Code';
      spyOn(component, 'applyCodeFilter');

      component.handleEnterFilterValue(filterValue);

      expect(component.applyCodeFilter).toHaveBeenCalledWith(filterValue);
    });

    it('should apply designation filter', () => {
      const filterValue = 'Test Designation';
      component.currentFilter = 'Designation';
      spyOn(component, 'applyDesignationFilter');

      component.handleEnterFilterValue(filterValue);

      expect(component.applyDesignationFilter).toHaveBeenCalledWith(filterValue);
    });
  });

  describe('handleResetFilter', () => {
    it('should reset filters and medical record conditions', () => {
      component.medicalRecordConditions = [{ conditionCode: 'C1', conditionDesignation: 'Test Designation', doctorName: 'Dr. Smith', comment: 'Test Comment' }];
      component.medicalRecordConditionsAux = [...component.medicalRecordConditions];
      component.currentFilter = 'Code';

      component.handleResetFilter();

      expect(component.medicalRecordConditions).toEqual(component.medicalRecordConditionsAux);
      expect(component.errorMessage).toBe('');
      expect(component.currentFilter).toBe('');
      expect(component.filteredMedicalRecordConditions).toBeNull();
    });
  });

  describe('applyCodeFilter', () => {
    it('should apply the code filter and update the medical record conditions', () => {
      const filterValue = 'C1';
      const mockFilteredConditions = {
        filteredMedicalRecordConditions: {
          conditionCode: 'C1',
          conditionDesignation: 'Test Designation',
          doctorName: 'Dr. Smith',
          comment: 'Test Comment'
        }
      };

      mockMedicalRecordConditionService.filterMedicalRecordConditionsByCode.and.returnValue(of(mockFilteredConditions));

      component.applyCodeFilter(filterValue);

      expect(mockMedicalRecordConditionService.filterMedicalRecordConditionsByCode).toHaveBeenCalledWith(component.medicalRecordId, filterValue);
      expect(component.medicalRecordConditions.length).toBeGreaterThan(0); // Check if conditions are updated
    });

    it('should handle errors during code filter', () => {
      const filterValue = 'C1';
      mockMedicalRecordConditionService.filterMedicalRecordConditionsByCode.and.returnValue(throwError('Error applying code filter'));

      component.applyCodeFilter(filterValue);

      expect(component.errorMessage).toBe('Error applying code filter');
    });
  });

  describe('applyDesignationFilter', () => {
    it('should apply the designation filter and update the medical record conditions', () => {
      const filterValue = 'Test Designation';
      const mockFilteredConditions = {
        filteredMedicalRecordConditions: {
          conditionCode: 'C1',
          conditionDesignation: 'Test Designation',
          doctorName: 'Dr. Smith',
          comment: 'Test Comment'
        }
      };

      mockMedicalRecordConditionService.filterMedicalRecordConditionsByDesignation.and.returnValue(of(mockFilteredConditions));

      component.applyDesignationFilter(filterValue);

      expect(mockMedicalRecordConditionService.filterMedicalRecordConditionsByDesignation).toHaveBeenCalledWith(component.medicalRecordId, filterValue);
      expect(component.medicalRecordConditions.length).toBeGreaterThan(0); // Check if conditions are updated
    });

    it('should handle errors during designation filter', () => {
      const filterValue = 'Test Designation';
      mockMedicalRecordConditionService.filterMedicalRecordConditionsByDesignation.and.returnValue(throwError('Error applying designation filter'));

      component.applyDesignationFilter(filterValue);

      expect(component.errorMessage).toBe('Error applying designation filter');
    });
  });




});
