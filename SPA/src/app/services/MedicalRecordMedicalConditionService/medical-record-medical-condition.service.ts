import { Injectable } from '@angular/core';
import json from '../../appsettings.json';
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, map, Observable, throwError} from 'rxjs';
import {DisplayMedicalRecordConditionDTO} from '../../DTOs/displayDTOs/displayMedicalRecordConditionDTO';
import {MedicalRecordConditionMapper} from '../../DTOs/mappers/medicalRecordConditionMapper';
import {BackendMedicalRecordConditionDTO} from '../../DTOs/backendDTOs/backendMedicalRecordConditionDTO';

@Injectable({
  providedIn: 'root'
})
export class MedicalRecordMedicalConditionService {

  private apiUrl = json.backendApi["2"].url + '/medicalRecord';

  constructor(private http: HttpClient) {
  }

  listMedicalRecordConditionsByMedicalRecordId(medicalRecordId: string) : Observable<{ medicalRecordConditions: DisplayMedicalRecordConditionDTO[] }>{
    const url = `${this.apiUrl}/${medicalRecordId}/condition`;

    return this.http
      .get<{ medicalRecordConditions: BackendMedicalRecordConditionDTO[] }>(url, {
      withCredentials: true
    }).pipe(
      map((response) => {

        const medicalRecordConditions: DisplayMedicalRecordConditionDTO[] = [];

        for (const backendMedicalRecordConditionDTO of response.medicalRecordConditions) {
          const medicalRecordConditionDomain = MedicalRecordConditionMapper.backendDisplayDTOToDomain(backendMedicalRecordConditionDTO);
          const displayMedicalRecordConditionDTO = MedicalRecordConditionMapper.domainToDisplayDTO(medicalRecordConditionDomain);

          medicalRecordConditions.push(displayMedicalRecordConditionDTO);
        }

        return { medicalRecordConditions: medicalRecordConditions };
      }),
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if(error.status === 850) {
          errorMessage = error.error.message;
        }

        if(error.status === 900) {
          errorMessage = "This section is empty due to not founding the medical record.";
        }

        return throwError(errorMessage);
      })
    );
  }

  filterMedicalRecordConditionsByCode(medicalRecordId: string, code: string) : Observable<{ filteredMedicalRecordConditions: DisplayMedicalRecordConditionDTO }> {
    const url = `${this.apiUrl}/${medicalRecordId}/condition/by-code/${code}`;

    return this.http
      .get<{ medicalRecordCondition: BackendMedicalRecordConditionDTO }>(url, {
        withCredentials: true
      }).pipe(
        map((response) => {

            const medicalRecordConditionDomain = MedicalRecordConditionMapper.backendDisplayDTOToDomain(response.medicalRecordCondition);
            const displayMedicalRecordConditionDTO = MedicalRecordConditionMapper.domainToDisplayDTO(medicalRecordConditionDomain);

          return { filteredMedicalRecordConditions: displayMedicalRecordConditionDTO };
        }),
        catchError((error: HttpErrorResponse) => {
          let errorMessage = 'An unknown error occurred.';

          if(error.status === 900) {
            errorMessage = error.error.message;
          }

          if(error.status >= 801 && error.status <= 809) {
            errorMessage = error.error.message;
          }

          if (error.status === 810) {
            errorMessage = error.error.message;
          }

          if (error.status === 851) {
            errorMessage = error.error.message;
          }

          console.log(error);

          return throwError(errorMessage);
        })
      );

  }

  filterMedicalRecordConditionsByDesignation(medicalRecordId: string, designation: string) : Observable<{ filteredMedicalRecordConditions: DisplayMedicalRecordConditionDTO }> {
    const url = `${this.apiUrl}/${medicalRecordId}/condition/by-designation/${designation}`;


    return this.http
      .get<{ medicalRecordCondition: BackendMedicalRecordConditionDTO }>(url, {
        withCredentials: true
      }).pipe(
        map((response) => {

          const medicalRecordConditionDomain = MedicalRecordConditionMapper.backendDisplayDTOToDomain(response.medicalRecordCondition);
          const displayMedicalRecordConditionDTO = MedicalRecordConditionMapper.domainToDisplayDTO(medicalRecordConditionDomain);

          return { filteredMedicalRecordConditions: displayMedicalRecordConditionDTO };
        }),
        catchError((error: HttpErrorResponse) => {
          let errorMessage = 'An unknown error occurred.';

          if(error.status === 900) {
            errorMessage = error.error.message;
          }

          if(error.status >= 801 && error.status <= 809) {
            errorMessage = error.error.message;
          }

          if (error.status === 810) {
            errorMessage = error.error.message;
          }

          if (error.status === 851) {
            errorMessage = error.error.message;
          }

          return throwError(errorMessage);
        })
      );

  }

}
